# Heatmap helper invariants (server.R, zero production changes).
# Guards regressions in: row z-scoring, group-split factors, seeded k-means
# slices, cluster-title sizing, legend-side sanitizing.

test_that("zscore_rows_safe standardizes rows and drops constant rows", {
  zscore_rows_safe <- eval_app_fn(SERVER_R, "zscore_rows_safe")

  set.seed(1)
  m <- rbind(matrix(rnorm(40, mean = 10, sd = 3), 4, 10),
             rep(5, 10))  # constant row
  rownames(m) <- paste0("f", 1:5)
  out <- zscore_rows_safe(m)

  expect_equal(nrow(out), 4)  # constant row removed
  expect_true(all(is.finite(out)))
  expect_equal(unname(rowMeans(out)), rep(0, 4), tolerance = 1e-8)
  expect_equal(unname(apply(out, 1, sd)), rep(1, 4), tolerance = 1e-8)
})

test_that("zscore_rows_safe is idempotent (no double-scaling risk)", {
  zscore_rows_safe <- eval_app_fn(SERVER_R, "zscore_rows_safe")

  set.seed(3)
  m <- matrix(rnorm(60, mean = 10, sd = 3), 6, 10)
  once <- zscore_rows_safe(m)
  twice <- zscore_rows_safe(once)
  expect_equal(max(abs(once - twice)), 0, tolerance = 1e-12)
})

test_that("make_group_split preserves spaces and level order", {
  preserve_group_names <- eval_app_fn(SERVER_R, "preserve_group_names",
                                      mocks = list(values = mock_values()))
  e <- new.env(parent = globalenv())
  assign("preserve_group_names", preserve_group_names, envir = e)
  assign("values", mock_values(), envir = e)
  eval(parse(text = extract_fn(SERVER_R, "make_group_split")), envir = e)
  make_group_split <- get("make_group_split", envir = e)

  sp <- c(rep("Day 1", 3), rep("Control group", 2))
  fs <- make_group_split(sp)
  expect_true(is.factor(fs))
  expect_equal(levels(fs), c("Day 1", "Control group"))
  expect_null(make_group_split(NULL))
})

test_that("cluster_rows_kmeans is stable, seeded, and RNG-clean", {
  cluster_rows_kmeans <- eval_app_fn(SERVER_R, "cluster_rows_kmeans")

  set.seed(11)
  m <- matrix(rnorm(200), 20, 10)
  r1 <- cluster_rows_kmeans(m, 4)
  r2 <- cluster_rows_kmeans(m, 4)
  expect_identical(as.character(r1), as.character(r2))  # redraw-stable
  expect_equal(levels(r1), paste("Cluster", 1:4))
  expect_null(cluster_rows_kmeans(m, 1))     # 1 = no split
  expect_null(cluster_rows_kmeans(m[1:2, ], 4))  # more clusters than rows

  # session RNG stream must be untouched
  set.seed(99)
  tmp <- runif(1)
  ref_next <- runif(1)
  set.seed(99)
  tmp <- runif(1)
  invisible(cluster_rows_kmeans(m, 4))
  expect_identical(runif(1), ref_next)
})

test_that("cluster_title_size caps downward with many slices", {
  cluster_title_size <- eval_app_fn(SERVER_R, "cluster_title_size")

  expect_equal(cluster_title_size(10, 1), 10)
  expect_equal(cluster_title_size(10, 4), 10)
  expect_lte(cluster_title_size(10, 8), 10)
  expect_gte(cluster_title_size(10, 12), 7)  # floor, never unreadable
  expect_equal(cluster_title_size(NA, 4), 10)  # bad input -> default
})

test_that("heatmap_legend_sides sanitizes to valid sides", {
  heatmap_legend_sides <- eval_app_fn(SERVER_R, "heatmap_legend_sides")

  s <- heatmap_legend_sides("left", "right")
  expect_equal(s$heatmap, "left")
  expect_equal(s$annotation, "right")
  s <- heatmap_legend_sides("bogus", NA)
  expect_equal(s$heatmap, "right")  # fallbacks, not errors
  expect_equal(s$annotation, "right")
})
