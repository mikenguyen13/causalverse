library(testthat)
library(causalverse)

# ---------------------------------------------------------------------------
# Helper: build a panel whose untreated counterfactual is an exact convex
# combination of two donors, so the ATT is recoverable analytically.
# ---------------------------------------------------------------------------
make_exact_panel <- function(N0 = 5, T0 = 20, T1 = 5, tau = 3, seed = 1) {
  set.seed(seed)
  Tt <- T0 + T1
  Yc <- matrix(rnorm(N0 * Tt), N0, Tt)
  y1 <- 0.5 * Yc[1, ] + 0.5 * Yc[2, ]          # exactly representable
  y1[(T0 + 1):Tt] <- y1[(T0 + 1):Tt] + tau     # constant treatment effect
  list(Y = rbind(Yc, y1), N0 = N0, T0 = T0, T1 = T1, tau = tau)
}

test_that("recovers a known ATT exactly when the counterfactual is representable", {
  p <- make_exact_panel(tau = 3)
  fit <- suppressWarnings(sc_debiased_ttest(p$Y, N0 = p$N0, T0 = p$T0, K = 4))

  # Pre-treatment fit is exact, so every fold-specific bias correction is zero
  # and each fold estimate equals the true ATT.
  expect_equal(fit$att, 3, tolerance = 1e-3)
  expect_true(all(abs(fit$tau_folds - 3) < 1e-3))
})

test_that("degenerate fold dispersion warns and returns NA inference", {
  p <- make_exact_panel(tau = 3)

  # With an exact pre-treatment fit the fold estimates coincide, so the
  # self-normalized standard error is numerically zero. The function must warn
  # and withhold the test rather than report near-infinite precision.
  expect_warning(
    fit <- sc_debiased_ttest(p$Y, N0 = p$N0, T0 = p$T0, K = 4),
    "not defined"
  )

  expect_lt(fit$se, 1e-6)
  expect_true(is.na(fit$t_stat))
  expect_true(is.na(fit$p_value))
  # The point estimate is still meaningful.
  expect_equal(fit$att, 3, tolerance = 1e-3)
})

test_that("returns the documented structure with correct dimensions", {
  p <- make_exact_panel()
  fit <- suppressWarnings(sc_debiased_ttest(p$Y, N0 = p$N0, T0 = p$T0, K = 5))

  expect_type(fit, "list")
  expect_named(
    fit,
    c("att", "se", "t_stat", "p_value", "ci", "df", "tau_folds",
      "weights", "att_naive", "K", "r", "alpha")
  )
  expect_length(fit$tau_folds, 5)
  expect_equal(dim(fit$weights), c(p$N0, 5))
  expect_equal(fit$df, 4)
  expect_equal(fit$r, min(p$T0 %/% 5, p$T1))
  expect_named(fit$ci, c("lower", "upper"))
})

test_that("fold weights lie on the unit simplex", {
  p <- make_exact_panel()
  fit <- suppressWarnings(sc_debiased_ttest(p$Y, N0 = p$N0, T0 = p$T0, K = 4))

  expect_true(all(fit$weights >= -1e-8))
  expect_true(all(abs(colSums(fit$weights) - 1) < 1e-6))
})

test_that("confidence interval is centered on the estimate and respects alpha", {
  set.seed(7)
  N0 <- 10; T0 <- 40; T1 <- 8; Tt <- T0 + T1
  Yc <- matrix(rnorm(N0 * Tt), N0, Tt)
  y1 <- colMeans(Yc[1:3, ]) + rnorm(Tt, sd = 0.2)
  y1[(T0 + 1):Tt] <- y1[(T0 + 1):Tt] + 2
  Y <- rbind(Yc, y1)

  fit90 <- sc_debiased_ttest(Y, N0 = N0, T0 = T0, K = 4, alpha = 0.10)
  fit95 <- sc_debiased_ttest(Y, N0 = N0, T0 = T0, K = 4, alpha = 0.05)

  expect_equal(mean(fit95$ci), fit95$att, tolerance = 1e-8)
  # A 95% interval must be wider than a 90% interval on the same data.
  expect_gt(diff(fit95$ci), diff(fit90$ci))
})

test_that("debiasing reduces bias relative to the undebiased SC estimator", {
  # DGP with an imperfect pre-treatment fit, which is what biases canonical SC.
  set.seed(2024)
  reps <- 60
  bias_deb <- numeric(reps)
  bias_naive <- numeric(reps)
  tau <- 1.5

  for (b in seq_len(reps)) {
    N0 <- 10; T0 <- 40; T1 <- 8; Tt <- T0 + T1
    f <- cumsum(rnorm(Tt))
    load_c <- runif(N0, 0.5, 1.5)
    Yc <- outer(load_c, f) + matrix(rnorm(N0 * Tt, sd = 0.5), N0, Tt)
    # Treated loading sits outside the donor convex hull span, so the fit is
    # imperfect and the naive estimator picks up pre-treatment gap bias.
    y1 <- 1.7 * f + rnorm(Tt, sd = 0.5)
    y1[(T0 + 1):Tt] <- y1[(T0 + 1):Tt] + tau
    Y <- rbind(Yc, y1)

    fit <- sc_debiased_ttest(Y, N0 = N0, T0 = T0, K = 4)
    bias_deb[b] <- fit$att - tau
    bias_naive[b] <- fit$att_naive - tau
  }

  expect_lt(abs(mean(bias_deb)), abs(mean(bias_naive)))
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("rejects more than one treated unit", {
  p <- make_exact_panel()
  Y2 <- rbind(p$Y, p$Y[nrow(p$Y), ])          # two treated rows
  expect_error(
    sc_debiased_ttest(Y2, N0 = p$N0, T0 = p$T0),
    "single treated unit"
  )
})

test_that("rejects K < 2", {
  p <- make_exact_panel()
  expect_error(sc_debiased_ttest(p$Y, N0 = p$N0, T0 = p$T0, K = 1), "K` must be")
})

test_that("rejects K larger than the number of pre-treatment periods", {
  p <- make_exact_panel(T0 = 4, T1 = 3)
  expect_error(
    sc_debiased_ttest(p$Y, N0 = p$N0, T0 = 4, K = 8),
    "Cross-fitting blocks are empty"
  )
})

test_that("rejects missing values and non-matrix input", {
  p <- make_exact_panel()
  Y_na <- p$Y
  Y_na[1, 1] <- NA
  expect_error(sc_debiased_ttest(Y_na, N0 = p$N0, T0 = p$T0), "missing values")
  expect_error(sc_debiased_ttest(as.data.frame(p$Y), N0 = p$N0, T0 = p$T0), "numeric matrix")
})

test_that("rejects out-of-range N0, T0, and alpha", {
  p <- make_exact_panel()
  expect_error(sc_debiased_ttest(p$Y, N0 = 1, T0 = p$T0), "N0` must be at least 2")
  expect_error(sc_debiased_ttest(p$Y, N0 = p$N0, T0 = ncol(p$Y)), "T0` must be at least 2")
  expect_error(sc_debiased_ttest(p$Y, N0 = p$N0, T0 = p$T0, alpha = 0), "alpha` must be")
  expect_error(sc_debiased_ttest(p$Y, N0 = p$N0, T0 = p$T0, alpha = 1), "alpha` must be")
})
