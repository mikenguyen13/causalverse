library(testthat)
library(causalverse)

# Helper: a DGP in which `x1`, `x2` drive the outcome and `z1`, `z2`, `z3` are
# pure noise. Which covariate drives assignment is controlled by `driver`.
bpt_dgp <- function(n = 600, driver = c("none", "prognostic", "noise"),
                    strength = 1.2, seed = 1) {
  driver <- match.arg(driver)
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n), x2 = rnorm(n),
    z1 = rnorm(n), z2 = rnorm(n), z3 = rnorm(n)
  )
  lin <- switch(driver,
    none       = rep(0, n),
    prognostic = strength * d$x1,
    noise      = strength * d$z1
  )
  d$treat <- rbinom(n, 1, plogis(lin))
  # Outcome depends only on x1 and x2 -- z1..z3 carry no prognostic content.
  d$y <- 1.5 * d$x1 - 1.0 * d$x2 + rnorm(n)
  d
}

bpt_covs <- c("x1", "x2", "z1", "z2", "z3")


test_that("balance_prognosis_test returns the documented structure", {
  d <- bpt_dgp(seed = 101)
  res <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                n_boot = 200, seed = 42)

  expect_type(res, "list")
  expect_true(all(c("delta_pw", "p_value", "delta_uw", "p_value_uw",
                    "prognosis_r2", "coefficients", "ci", "se", "draws",
                    "n_treated", "n_control", "method", "basis",
                    "n_boot") %in% names(res)))

  expect_length(res$delta_pw, 1L)
  expect_true(is.finite(res$delta_pw))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  expect_true(res$prognosis_r2 >= 0 && res$prognosis_r2 <= 1)

  expect_s3_class(res$coefficients, "data.frame")
  expect_equal(nrow(res$coefficients), length(bpt_covs))
  expect_true(all(c("term", "prognosis", "smd", "contribution") %in%
                    names(res$coefficients)))
  expect_setequal(res$coefficients$term, bpt_covs)

  # The statistic is exactly the sum of the per-term contributions.
  expect_equal(res$delta_pw, sum(res$coefficients$contribution),
               tolerance = 1e-8)
  # ... and the unweighted statistic is the sum of the SMDs.
  expect_equal(res$delta_uw, sum(res$coefficients$smd), tolerance = 1e-8)

  expect_equal(res$n_treated + res$n_control, nrow(d))
  expect_named(res$ci, c("lower", "upper"))
  expect_lt(res$ci[["lower"]], res$ci[["upper"]])
})


test_that("the test does not reject when as-if random assignment holds", {
  d <- bpt_dgp(driver = "none", seed = 202)
  res <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                n_boot = 500, seed = 42)

  # Assignment is a fair coin, so the prognosis-weighted imbalance is ~0.
  expect_lt(abs(res$delta_pw), 0.25)
  expect_gt(res$p_value, 0.10)
  # The 95% interval should cover zero.
  expect_lt(res$ci[["lower"]], 0)
  expect_gt(res$ci[["upper"]], 0)
})


test_that("the test rejects when assignment depends on a prognostic covariate", {
  d <- bpt_dgp(driver = "prognostic", seed = 303)
  res <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                n_boot = 500, seed = 42)

  expect_gt(abs(res$delta_pw), 0.30)
  expect_lt(res$p_value, 0.05)

  # x1 must dominate the decomposition.
  top <- res$coefficients$term[which.max(abs(res$coefficients$contribution))]
  expect_equal(top, "x1")
})


test_that("prognosis weighting ignores imbalance on non-prognostic covariates", {
  # This is the central claim of Bicalho, Bouyamourn & Dunning (2026): when the
  # imbalanced covariate carries no outcome information, the design is not
  # actually threatened, and a prognosis-weighted test should stay quiet where
  # an unweighted test raises a false positive.
  d <- bpt_dgp(driver = "noise", strength = 1.6, seed = 404)
  res <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                n_boot = 500, seed = 42)

  # z1 is badly imbalanced by construction ...
  z1_smd <- res$coefficients$smd[res$coefficients$term == "z1"]
  expect_gt(abs(z1_smd), 0.5)

  # ... but carries almost no prognosis weight, so it barely moves delta_pw.
  z1_w <- res$coefficients$prognosis[res$coefficients$term == "z1"]
  expect_lt(abs(z1_w), 0.15)

  # The weighted statistic is far smaller in magnitude than the unweighted one.
  expect_lt(abs(res$delta_pw), abs(res$delta_uw))
  expect_gt(res$p_value, res$p_value_uw)
})


test_that("prognosis weights recover the outcome model's signal covariates", {
  d <- bpt_dgp(driver = "none", seed = 505)
  res <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                n_boot = 100, seed = 42)

  w <- setNames(abs(res$coefficients$prognosis), res$coefficients$term)
  # x1 (coef 1.5) and x2 (coef -1.0) must outrank every noise covariate.
  expect_gt(min(w[["x1"]], w[["x2"]]), max(w[["z1"]], w[["z2"]], w[["z3"]]))
  # R-squared should be high: the outcome is almost entirely explained by x1, x2.
  expect_gt(res$prognosis_r2, 0.5)
})


test_that("the permutation reference distribution behaves like the bootstrap", {
  d <- bpt_dgp(driver = "prognostic", seed = 606)
  res <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                method = "permutation", n_boot = 300, seed = 42)

  expect_equal(res$method, "permutation")
  expect_null(res$ci)              # permutation gives a p-value, not a CI
  expect_lt(res$p_value, 0.05)

  # Point estimate does not depend on the reference distribution used.
  res_b <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                  method = "bootstrap", n_boot = 300, seed = 42)
  expect_equal(res$delta_pw, res_b$delta_pw, tolerance = 1e-10)

  # Permutation draws are centred on zero under the sharp null.
  expect_lt(abs(mean(res$draws)), 0.15)
})


test_that("basis expansion adds polynomial and interaction terms", {
  d <- bpt_dgp(seed = 707)
  covs <- c("x1", "x2", "z1")

  n_lin  <- nrow(balance_prognosis_test(d, "treat", "y", covariates = covs,
                                        basis = "linear", n_boot = 50,
                                        seed = 1)$coefficients)
  n_poly <- nrow(balance_prognosis_test(d, "treat", "y", covariates = covs,
                                        basis = "poly", n_boot = 50,
                                        seed = 1)$coefficients)
  n_int  <- nrow(balance_prognosis_test(d, "treat", "y", covariates = covs,
                                        basis = "interaction", n_boot = 50,
                                        seed = 1)$coefficients)
  n_full <- nrow(balance_prognosis_test(d, "treat", "y", covariates = covs,
                                        basis = "full", n_boot = 50,
                                        seed = 1)$coefficients)

  expect_equal(n_lin, 3L)
  expect_equal(n_poly, 6L)   # 3 linear + 3 squares
  expect_equal(n_int, 6L)    # 3 linear + 3 pairwise products
  expect_equal(n_full, 9L)   # 3 linear + 3 squares + 3 products
})


test_that("a binary covariate contributes no squared term", {
  d <- bpt_dgp(seed = 808)
  d$bin <- rbinom(nrow(d), 1, 0.5)
  res <- balance_prognosis_test(d, "treat", "y",
                                covariates = c("x1", "bin"),
                                basis = "poly", n_boot = 50, seed = 1)
  # x1 gets I(x1^2); the binary covariate does not, since bin^2 == bin.
  expect_true("I(x1^2)" %in% res$coefficients$term)
  expect_false("I(bin^2)" %in% res$coefficients$term)
  expect_equal(nrow(res$coefficients), 3L)
})


test_that("clustered resampling runs and preserves the point estimate", {
  d <- bpt_dgp(n = 600, driver = "prognostic", seed = 909)
  d$cl <- rep(seq_len(120), each = 5)

  res_cl <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                   cluster = "cl", n_boot = 200, seed = 42)
  res_iid <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                    n_boot = 200, seed = 42)

  # Clustering changes the reference distribution, not the statistic itself.
  expect_equal(res_cl$delta_pw, res_iid$delta_pw, tolerance = 1e-10)
  expect_true(is.finite(res_cl$se))

  # Passing the cluster vector directly is equivalent to naming the column.
  res_vec <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                    cluster = d$cl, n_boot = 200, seed = 42)
  expect_equal(res_cl$delta_pw, res_vec$delta_pw, tolerance = 1e-10)
})


test_that("results are reproducible given a seed", {
  d <- bpt_dgp(seed = 111)
  a <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                              n_boot = 150, seed = 2024)
  b <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                              n_boot = 150, seed = 2024)
  expect_equal(a$p_value, b$p_value)
  expect_equal(a$draws, b$draws)
  expect_equal(a$ci, b$ci)
})


test_that("covariates default to all numeric columns except treatment/outcome", {
  d <- bpt_dgp(seed = 222)
  res <- balance_prognosis_test(d, "treat", "y", n_boot = 50, seed = 1)
  expect_setequal(res$coefficients$term, bpt_covs)
})


test_that("a weakly prognostic covariate set triggers the R-squared warning", {
  d <- bpt_dgp(seed = 333)
  d$y <- rnorm(nrow(d))          # outcome unrelated to every covariate
  expect_warning(
    balance_prognosis_test(d, "treat", "y", covariates = c("z1", "z2", "z3"),
                           n_boot = 50, seed = 1),
    "Prognosis R-squared"
  )
  # The warning can be switched off.
  expect_silent(
    balance_prognosis_test(d, "treat", "y", covariates = c("z1", "z2", "z3"),
                           n_boot = 50, seed = 1, r2_warn = 0)
  )
})


test_that("missing values are dropped with a warning", {
  d <- bpt_dgp(seed = 444)
  d$x1[1:10] <- NA
  expect_warning(
    res <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                  n_boot = 50, seed = 1),
    "row\\(s\\) dropped"
  )
  expect_equal(res$n_dropped, 10L)
  expect_equal(res$n_treated + res$n_control, nrow(d) - 10L)
})


test_that("input validation rejects malformed arguments", {
  d <- bpt_dgp(seed = 555)

  expect_error(balance_prognosis_test(as.matrix(d), "treat", "y"),
               "must be a data frame")
  expect_error(balance_prognosis_test(d, "nope", "y"),
               "not found in data")
  expect_error(balance_prognosis_test(d, "treat", "nope"),
               "not found in data")
  expect_error(balance_prognosis_test(d, "treat", "y", covariates = "ghost"),
               "Covariates not found")
  expect_error(balance_prognosis_test(d, "treat", "y", n_boot = 0),
               "positive integer")
  expect_error(balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                      n_boot = 10, seed = "abc"),
               "single number")

  # Non-numeric outcome and non-numeric covariates are refused.
  d_chr <- d; d_chr$y <- as.character(d_chr$y)
  expect_error(balance_prognosis_test(d_chr, "treat", "y",
                                      covariates = bpt_covs),
               "must be numeric")
  d_fac <- d; d_fac$grp <- factor(sample(letters[1:3], nrow(d), TRUE))
  expect_error(balance_prognosis_test(d_fac, "treat", "y",
                                      covariates = c("x1", "grp")),
               "must be numeric or logical")

  # A constant treatment is not a design.
  d_const <- d; d_const$treat <- 1L
  expect_error(balance_prognosis_test(d_const, "treat", "y",
                                      covariates = bpt_covs),
               "both values")

  # More terms than control units cannot identify the prognosis regression.
  d_small <- d[c(which(d$treat == 1)[1:40], which(d$treat == 0)[1:3]), ]
  expect_error(balance_prognosis_test(d_small, "treat", "y",
                                      covariates = bpt_covs),
               "more control units than terms")
})


test_that("logical and two-level factor treatments are accepted", {
  d <- bpt_dgp(driver = "prognostic", seed = 666)
  ref <- balance_prognosis_test(d, "treat", "y", covariates = bpt_covs,
                                n_boot = 100, seed = 7)

  d_lgl <- d; d_lgl$treat <- as.logical(d_lgl$treat)
  d_fac <- d; d_fac$treat <- factor(d_fac$treat, levels = c(0, 1))

  expect_equal(
    balance_prognosis_test(d_lgl, "treat", "y", covariates = bpt_covs,
                           n_boot = 100, seed = 7)$delta_pw,
    ref$delta_pw, tolerance = 1e-10)
  expect_equal(
    balance_prognosis_test(d_fac, "treat", "y", covariates = bpt_covs,
                           n_boot = 100, seed = 7)$delta_pw,
    ref$delta_pw, tolerance = 1e-10)

  # A three-level factor is not a binary design.
  d_bad <- d; d_bad$treat <- factor(sample(letters[1:3], nrow(d), TRUE))
  expect_error(balance_prognosis_test(d_bad, "treat", "y",
                                      covariates = bpt_covs),
               "must be binary")
})


test_that("a duplicated covariate is dropped as collinear", {
  d <- bpt_dgp(seed = 777)
  d$x1_copy <- d$x1
  expect_warning(
    res <- balance_prognosis_test(d, "treat", "y",
                                  covariates = c("x1", "x1_copy", "x2"),
                                  n_boot = 50, seed = 1),
    "collinear term"
  )
  expect_true(is.finite(res$delta_pw))
})
