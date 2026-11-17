library(testthat)
library(causalverse)

test_that("shift_test keeps its size under the sharp null", {
  skip_on_cran()
  set.seed(101)
  rej <- replicate(200, {
    x <- sample(rep(0:1, 50))
    shift_test(rnorm(100), x, B = 199)$p.value < 0.05
  })
  expect_lt(mean(rej), 0.10)
})

test_that("shift_test detects a variance-only (cancelling) shift", {
  set.seed(102)
  x <- rep(0:1, each = 300)
  s <- sample(c(-1, 1), 600, replace = TRUE)
  z <- rnorm(600) + 1.2 * x * s
  res <- shift_test(z, x, B = 499, seed = 1)
  expect_s3_class(res, "shift_test")
  expect_lt(res$p.value, 0.05)
  # a mean-only test should not see it
  res_mean <- shift_test(z, x, stats = "mean", B = 499, seed = 1)
  expect_gt(res_mean$p.value, res$p.value)
})

test_that("shift_test handles ties and missing values without false rejection", {
  set.seed(103)
  n <- 800
  x <- sample(0:1, n, replace = TRUE)
  x[sample(n, 30)] <- NA
  z <- sample(1:7, n, replace = TRUE)          # Likert-type, heavy ties
  z[sample(n, 20)] <- NA
  # sort rows by treatment so that within-tie order is correlated with x
  o <- order(x, na.last = TRUE); x <- x[o]; z <- z[o]
  res <- shift_test(z, x, B = 999, seed = 2)
  expect_gt(res$p.value, 0.01)
  expect_equal(res$n1 + res$n0, sum(!is.na(x) & !is.na(z)))
})

test_that("baseline residualization makes the test far more powerful", {
  set.seed(104)
  n <- 200
  x <- rep(0:1, each = n / 2)
  z0 <- rnorm(n)
  z1 <- z0 + 0.15 * x + 0.1 * rnorm(n)
  p_post <- shift_test(z1, x, B = 499, seed = 3)$p.value
  p_pp <- shift_test(z1, x, z_pre = z0, B = 499, seed = 3)$p.value
  expect_lt(p_pp, 0.01)
  expect_lt(p_pp, p_post)
})

test_that("equivalence_shift certifies a null shift in a large sample", {
  set.seed(105)
  x <- rep(0:1, each = 2000)
  eq <- equivalence_shift(rnorm(4000), x, margin = 0.2)
  expect_true(eq$equivalent)
  eq2 <- equivalence_shift(rnorm(4000) + 0.5 * x, x, margin = 0.2)
  expect_false(eq2$equivalent)
})

test_that("treatment coding is validated", {
  expect_error(shift_test(rnorm(10), rep(1:5, 2)), "binary")
  expect_error(shift_test(rnorm(10), rep(c("a", "b", "c", "d", "e"), 2)), "two levels")
  expect_silent(shift_test(rnorm(20), rep(c(TRUE, FALSE), 10), B = 19))
})

test_that("role_test recovers the role in large samples", {
  skip_on_cran()
  v <- function(world, pre = TRUE) {
    d <- simulate_role_data(world, n = 3000, seed = 7)
    role_test(d, "X", "Y", "Z_post", z_pre = if (pre) "Z_pre", B = 499, seed = 8)$verdict
  }
  expect_equal(v("moderator"), "moderator")
  expect_equal(v("mediator"), "mediator")
  expect_equal(v("both"), "both")
  expect_equal(v("neither"), "neither")
  expect_false(v("cancelling") == "moderator")
  expect_false(v("cancelling", pre = FALSE) == "moderator")
})

test_that("role_test output structure", {
  d <- simulate_role_data("mediator", n = 400, seed = 9)
  r <- role_test(d, "X", "Y", "Z_post", B = 199, seed = 1)
  expect_s3_class(r, "role_test")
  expect_equal(r$design, "post-only")
  expect_equal(nrow(r$steps), 4)
  expect_output(print(r), "Verdict")
  expect_error(role_test(d, "X", "Y", "nope"), "Not found")
})

test_that("simulate_role_data returns the documented structure", {
  d <- simulate_role_data("curved_mediator", n = 100, seed = 1)
  expect_named(d, c("X", "Z_pre", "Z_post", "Y"))
  expect_equal(attr(d, "truth"), "both")
  expect_equal(sum(d$X), 50)
})

test_that("role_simulation and its summary run", {
  skip_on_cran()
  s <- role_simulation(worlds = c("moderator", "cancelling"), n = 200, reps = 5, B = 99, seed = 1)
  expect_s3_class(s, "role_simulation")
  expect_equal(nrow(s), 2 * 5 * 3)
  sm <- summary(s)
  expect_named(sm, c("world", "truth", "rule", "pct_correct", "pct_moderator", "pct_undetermined"))
  expect_equal(nrow(sm), 6)
})

test_that("role_bounds wraps prepost", {
  skip_if_not_installed("prepost")
  skip_on_cran()
  data("land_experiment", package = "prepost", envir = environment())
  post <- subset(land_experiment, prepost == 1)
  b <- role_bounds(post, "treat_comb", "support", "land_insecure", design = "post", sims = 50)
  expect_true(all(c("lower", "upper") %in% names(b)))
  expect_lt(b$lower, b$upper)
})

test_that("equivalence_shift validates its inputs", {
  x <- rep(0:1, 50)
  expect_error(equivalence_shift(rnorm(99), x), "same length")
  expect_error(equivalence_shift(as.character(rnorm(100)), x), "numeric")
  expect_error(equivalence_shift(rep(1, 100), x), "no variation")
  expect_error(equivalence_shift(rnorm(100), x, margin = -1), "margin")
  expect_error(equivalence_shift(rnorm(100), x, alpha = 2), "alpha")
  expect_error(equivalence_shift(rnorm(100), x, z_pre = rnorm(5)), "same length")
})

test_that("too few permutations to ever reject is flagged", {
  d <- simulate_role_data("mediator", n = 200, seed = 11)
  expect_warning(role_test(d, "X", "Y", "Z_post", B = 19), "never reject")
  expect_error(shift_test(rnorm(20), rep(0:1, 10), B = 0), "positive integer")
  expect_error(role_test(as.matrix(d), "X", "Y", "Z_post"), "data frame")
})
