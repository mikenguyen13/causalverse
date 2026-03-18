library(testthat)
library(causalverse)

test_that("med_ind returns correct types", {
  result <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01, seed = 1)

  expect_true(is.list(result))
  expect_true(all(c("lower_quantile", "upper_quantile", "raw_data", "plot") %in% names(result)))
  expect_true(is.numeric(result$lower_quantile))
  expect_true(is.numeric(result$upper_quantile))
  expect_true(is.numeric(result$raw_data))
  expect_true(ggplot2::is_ggplot(result$plot))
})

test_that("med_ind returns expected values with set seed", {
  result <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01, seed = 1)

  expect_true(result$lower_quantile > 0)
  expect_true(result$upper_quantile > result$lower_quantile)
  expect_equal(length(result$raw_data), 20000)
})

test_that("med_ind is reproducible with same seed", {
  result1 <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01, seed = 42)
  result2 <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01, seed = 42)

  expect_equal(result1$lower_quantile, result2$lower_quantile)
  expect_equal(result1$upper_quantile, result2$upper_quantile)
})

test_that("med_ind respects custom CI level", {
  result_90 <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01, ci = 90, seed = 1)
  result_99 <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01, ci = 99, seed = 1)

  # 99% CI should be wider than 90% CI
  width_90 <- result_90$upper_quantile - result_90$lower_quantile
  width_99 <- result_99$upper_quantile - result_99$lower_quantile
  expect_true(width_99 > width_90)
})

test_that("med_ind respects custom iterations", {
  result <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01,
                    iterations = 5000, seed = 1)

  expect_equal(length(result$raw_data), 5000)
})

test_that("med_ind handles zero effect correctly", {
  # When a=0 or b=0, indirect effect should be centered around 0
  result <- med_ind(a = 0, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0, seed = 1)

  expect_true(result$lower_quantile < 0)
  expect_true(result$upper_quantile > 0)
})

test_that("med_ind handles negative coefficients", {
  result <- med_ind(a = -0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01, seed = 1)

  # Negative a * positive b -> negative indirect effect
  expect_true(result$upper_quantile < 0)
})
