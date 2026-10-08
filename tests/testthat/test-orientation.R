# Fold-change orientation invariants.
# Every tab must agree: positive Log2FC = higher in Group 1 of the picked
# comparison. get_foldchange_column() flags reversed storage with a
# "REVERSED:" prefix that consumers must strip and negate. These tests pin
# that contract using the exact code pattern all consumers share.

orient_fc <- function(stat_results, fc_column_result) {
  # Mirrors the consumer pattern in server.R (heatmap/diff/enrichment/ROC/...)
  is_reversed <- isTRUE(grepl("^REVERSED:", fc_column_result %||% ""))
  fc_column <- if (!is.null(fc_column_result)) sub("^REVERSED:", "", fc_column_result) else NULL
  if (is.null(fc_column) || !fc_column %in% colnames(stat_results)) return(NULL)
  log2_fc <- stat_results[[fc_column]]
  if (is_reversed) log2_fc <- -log2_fc
  log2_fc
}

test_that("forward comparison passes through untouched", {
  sr <- data.frame(A_vs_B_Log2FC = c(2.0, -1.5, 0.2))
  expect_equal(orient_fc(sr, "A_vs_B_Log2FC"), c(2.0, -1.5, 0.2))
})

test_that("reversed storage is stripped and negated (Group1-positive)", {
  # Stored as B-vs-A: f1 higher in B (+2). Picked as A-vs-B, f1 must be -2.
  sr <- data.frame(B_vs_A_Log2FC = c(2.0, -1.5, 0.2))
  got <- orient_fc(sr, "REVERSED:B_vs_A_Log2FC")
  expect_equal(got, c(-2.0, 1.5, -0.2))

  feats <- c("f1", "f2", "f3")
  up_in_a <- feats[!is.na(got) & got >= 1.0]
  expect_equal(up_in_a, "f2")  # f2 higher in A, correctly recovered
})

test_that("missing columns resolve to NULL, never crash", {
  sr <- data.frame(A_vs_B_Log2FC = c(1.0))
  expect_null(orient_fc(sr, NULL))
  expect_null(orient_fc(sr, "REVERSED:Nope_Log2FC"))
  expect_null(orient_fc(sr, "Nope_Log2FC"))
})
