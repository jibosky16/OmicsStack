#!/usr/bin/env Rscript
# JS syntax gate for inline tags$script(HTML("...")) blocks in ui.R.
# A single JS syntax error kills its entire <script> tag in the browser
# (handlers never bind, with zero console-independent signal from R),
# so this script extracts every inline block and runs node --check on it.
# Usage: Rscript scripts/check_js.R  (exit 0 = clean; skips if node missing)

ui_path <- "ui.R"
if (!file.exists(ui_path) && file.exists("OmicsStack/ui.R")) ui_path <- "OmicsStack/ui.R"

exprs <- parse(ui_path)
scripts <- list()
walk <- function(x) {
  if (is.call(x)) {
    head <- x[[1]]
    is_tag_script <- identical(head, quote(script)) ||
      (is.call(head) && length(head) == 3L &&
       identical(head[[1]], quote(`$`)) &&
       identical(head[[2]], quote(tags)) &&
       identical(head[[3]], quote(script)))
    if (is_tag_script && length(x) >= 2L) {
      arg1 <- x[[2]]
      if (is.call(arg1) && identical(arg1[[1]], quote(HTML)) &&
          length(arg1) >= 2L && is.character(arg1[[2]])) {
        scripts[[length(scripts) + 1L]] <<- arg1[[2]]
      }
    }
    for (i in seq_along(x)) walk(x[[i]])
  } else if (is.pairlist(x) || is.list(x)) {
    for (el in as.list(x)) walk(el)
  }
}
for (expr in exprs) walk(expr)
cat(sprintf("Found %d inline script blocks in %s\n", length(scripts), ui_path))

node <- Sys.which("node")
if (!nzchar(node)) {
  cat("node not available; skipping JS syntax check (CI enforces it).\n")
  quit(status = 0)
}

dir.create(tmp <- tempfile(), showWarnings = FALSE)
bad <- 0L
for (i in seq_along(scripts)) {
  f <- file.path(tmp, sprintf("block%02d.js", i))
  writeLines(scripts[[i]], f, useBytes = TRUE)
  st <- suppressWarnings(system2(node, c("--check", f), stdout = TRUE, stderr = TRUE))
  code <- attr(st, "status")
  if (!is.null(code) && code != 0L) {
    bad <- bad + 1L
    cat(sprintf("SYNTAX ERROR in block %02d:\n%s\n", i, paste(st, collapse = "\n")))
  }
}
if (bad > 0L) {
  cat(sprintf("JS syntax gate FAILED: %d bad block(s)\n", bad))
  quit(status = 1)
}
cat("JS syntax gate passed: all blocks clean\n")
