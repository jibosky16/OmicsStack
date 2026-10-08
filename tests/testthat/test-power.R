# Power-analysis invariants (server.R patterns + pwrss API contract).
# Guards the exact failure modes found in audit: wrong pwrss argument names,
# wrong n semantics, wrong fallback closed form, raw-p stars.

test_that("pwrss t-test call uses valid args and sane values", {
  testthat::skip_if_not_installed("pwrss")
  p <- suppressMessages(
    pwrss::pwrss.t.2means(mu1 = 0, mu2 = 0.8, sd1 = 1, sd2 = 1,
                          alpha = 0.05, n2 = 25,
                          alternative = "not equal", verbose = FALSE)
  )[["power"]]
  expect_true(is.numeric(p) && is.finite(p))
  expect_equal(p, 0.7915, tolerance = 1e-3)  # Welch-adjusted textbook value
})

test_that("pwrss f.ancova takes n.levels + total N", {
  testthat::skip_if_not_installed("pwrss")
  p <- suppressMessages(
    pwrss::pwrss.f.ancova(eta2 = 0.1, alpha = 0.05, n.levels = 3,
                          n = 30, verbose = FALSE)
  )[["power"]]
  f2 <- 0.1 / 0.9
  expect_equal(p, 1 - pf(qf(0.95, 2, 27), 2, 27, ncp = f2 * 30), tolerance = 1e-6)
})

test_that("fallback normal-approx closed form is exact", {
  z <- qnorm(1 - 0.05 / 2)
  for (ncp in c(0, 0.5, 2.83, 5)) {
    got <- pnorm(ncp - z) + pnorm(-z - ncp)
    # Monte-Carlo cross-check of the closed form
    set.seed(42)
    mc <- mean(abs(rnorm(200000, mean = ncp)) > z)
    expect_equal(got, mc, tolerance = 5e-3)
    if (ncp == 0) expect_equal(got, 0.05, tolerance = 1e-10)  # = alpha at null
  }
})

test_that("BH stars stay quiet on pure noise (correlation/glycan scale)", {
  set.seed(4)
  m <- matrix(rnorm(50 * 16), 50, 16)
  n <- nrow(m)
  pv <- matrix(NA_real_, n, n)
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    t <- suppressWarnings(cor.test(m[i, ], m[j, ]))
    pv[i, j] <- t$p.value
    pv[j, i] <- t$p.value
  }
  raw_hits <- sum(pv[upper.tri(pv)] < 0.05, na.rm = TRUE)
  adj <- pv
  adj[upper.tri(adj)] <- p.adjust(pv[upper.tri(pv)], method = "BH")
  adj[lower.tri(adj)] <- t(adj)[lower.tri(adj)]
  bh_hits <- sum(adj[upper.tri(adj)] < 0.05, na.rm = TRUE)
  expect_gt(raw_hits, 0)  # old logic would decorate noise...
  expect_equal(bh_hits, 0)  # ...new logic stays silent
})

test_that("power worker is deterministic and well-formed (parallel refactor)", {
  worker <- eval_app_fn(SERVER_R, "compute_one_feature")

  set.seed(5)
  nf <- 8
  args <- list(
    fnames = paste0("f", seq_len(nf)),
    g1m = rnorm(nf, 10, 2), g1s = runif(nf, 0.5, 2),
    g2m = rnorm(nf, 11, 2), g2s = runif(nf, 0.5, 2),
    esizes = c(0.5, 1.0), nsizes = c(10, 20),
    alpha = 0.05, parametric = TRUE,
    cn1 = 8, cn2 = 8, tfrac = 0.8, acc_ids = NULL
  )
  run_all <- function() {
    lapply(seq_len(nf), function(fi) do.call(worker, c(list(fi = fi), args)))
  }
  r1 <- run_all()
  r2 <- run_all()
  flat <- function(res, key) {
    out <- list()
    for (x in res) {
      if (is.null(x)) next
      out <- c(out, x[[key]])
    }
    do.call(rbind, out)
  }
  # identical across repeated runs (no hidden RNG/state dependence)
  expect_identical(flat(r1, "current"), flat(r2, "current"))
  expect_identical(flat(r1, "detail"), flat(r2, "detail"))
  expect_identical(flat(r1, "summary"), flat(r2, "summary"))

  cur <- flat(r1, "current")
  expect_equal(nrow(cur), nf * 2)  # features x effect sizes
  expect_true(all(cur$CurrentPower >= 0 & cur$CurrentPower <= 1, na.rm = TRUE))
  expect_equal(sort(unique(cur$EffectSize)), c(0.5, 1.0))
})
