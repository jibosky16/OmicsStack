# Metabolite enrichment universe invariants (global.R).
# A hypergeometric universe smaller than the pathway itself (small targeted
# panels vs large KEGG pathways) makes phyper return NaN. The implementation
# must fall back to the full KEGG universe per pathway and disclose it.

test_that("small-panel universe falls back instead of NaN", {
  testthat::skip_if_not_installed("dplyr")
  e <- new.env(parent = globalenv())
  e$get_kegg_pathway_sizes <- function(ids) c(map00010 = 80, map00020 = 100)[ids]
  eval(parse(text = extract_fn(GLOBAL_R, "calculate_pathway_enrichment")), envir = e)
  calc <- get("calculate_pathway_enrichment", envir = e)

  pdf <- data.frame(
    kegg_id = c("C00001", "C00002", "C00003", "C00004"),
    pathway_id = c("map00010", "map00010", "map00020", "map00020"),
    pathway_name = c("P1", "P1", "P2", "P2"),
    stringsAsFactors = FALSE
  )

  # Coherent universe: used as-is
  r1 <- suppressMessages(calc(pdf, total_measured = 60, background_size = 500))
  expect_true(all(is.finite(r1$p_value)))
  expect_true(all(r1$background_N == 500))

  # Incoherent universe (N < M): fallback, finite p-values, disclosed
  r2 <- suppressMessages(calc(pdf, total_measured = 60, background_size = 60))
  expect_true(all(is.finite(r2$p_value)))
  expect_true(all(r2$background_N == 6610))
  expect_true("background_N" %in% names(r2))  # survives display allowlist
})

test_that("hypergeometric math matches phyper directly", {
  testthat::skip_if_not_installed("dplyr")
  e <- new.env(parent = globalenv())
  e$get_kegg_pathway_sizes <- function(ids) c(map00010 = 80)[ids]
  eval(parse(text = extract_fn(GLOBAL_R, "calculate_pathway_enrichment")), envir = e)
  calc <- get("calculate_pathway_enrichment", envir = e)

  pdf <- data.frame(
    kegg_id = c("C00001", "C00002"),
    pathway_id = c("map00010", "map00010"),
    pathway_name = c("P1", "P1"),
    stringsAsFactors = FALSE
  )
  r <- suppressMessages(calc(pdf, total_measured = 50, background_size = 1000))
  # k=2, M=80, N=1000, n=50
  expect_equal(r$p_value, phyper(1, 80, 920, 50, lower.tail = FALSE))
  expect_equal(r$enrichment_ratio, round((2 / 50) / (80 / 1000), 2))
})
