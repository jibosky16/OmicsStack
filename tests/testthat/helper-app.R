# Shared helpers for the Phase 1 suite.
#
# The app keeps its logic nested inside server()/global.R, so instead of
# sourcing those files (heavy side effects), tests extract individual
# top-level or nested function definitions by balanced-brace parsing and
# evaluate them in a controlled environment with stubbed dependencies.

APP_ROOT <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                          mustWork = FALSE)
SERVER_R <- file.path(APP_ROOT, "server.R")
GLOBAL_R <- file.path(APP_ROOT, "global.R")

# Extract the source text of `fname <- function...` (balanced braces).
extract_fn <- function(file, fname) {
  ll <- readLines(file, warn = FALSE)
  start <- grep(paste0("^\\s*", fname, " <- function"), ll)
  if (length(start) == 0) stop("function not found: ", fname, " in ", file)
  txt <- paste(ll[start[1]:length(ll)], collapse = "\n")
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  depth <- 0L
  started <- FALSE
  in_str <- FALSE
  quote_ch <- ""
  in_comment <- FALSE
  i <- 1L
  n <- length(chars)
  while (i <= n) {
    ch <- chars[i]
    if (in_comment) {
      if (ch == "\n") in_comment <- FALSE
    } else if (in_str) {
      if (ch == "\\") {
        i <- i + 1L  # skip escaped char
      } else if (ch == quote_ch) {
        in_str <- FALSE
      }
    } else if (ch == "#" ) {
      in_comment <- TRUE
    } else if (ch == "'" || ch == '"') {
      in_str <- TRUE
      quote_ch <- ch
    } else if (ch == "{") {
      depth <- depth + 1L
      started <- TRUE
    } else if (ch == "}") {
      depth <- depth - 1L
      if (started && depth == 0L) {
        return(paste(chars[1:i], collapse = ""))
      }
    }
    i <- i + 1L
  }
  stop("unbalanced braces extracting: ", fname)
}

# Evaluate an extracted function in a fresh env. `mocks` is a named list of
# stub objects the function may reference (e.g. a fake `values` list).
eval_app_fn <- function(file, fname, mocks = list()) {
  env <- new.env(parent = globalenv())
  for (nm in names(mocks)) assign(nm, mocks[[nm]], envir = env)
  eval(parse(text = extract_fn(file, fname)), envir = env)
  get(fname, envir = env)
}

# Mock Shiny `values` store: preserve_group_names() only reads
# values$unique_groups_original, which is NULL here (no sanitized names).
mock_values <- function() list(unique_groups_original = NULL)
