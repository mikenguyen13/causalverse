library(testthat)
library(causalverse)
library(fixest)

data(base_stagg)

test_that("stack_data function processes correctly", {
  stacked_data <- stack_data("year_treated", "year", 3, 3, base_stagg)
  expect_true(is.data.frame(stacked_data))

  # Test for the presence of df and rel_period columns
  expect_true("df" %in% colnames(stacked_data))
  expect_true("rel_period" %in% colnames(stacked_data))

  # Test that the control group value is not in df
  expect_false(10000 %in% stacked_data$df)
})

test_that("stack_data creates relative period dummy columns", {
  stacked_data <- stack_data("year_treated", "year", 2, 2, base_stagg)

  # Should have rel_period_-2, rel_period_-1, rel_period_0, rel_period_1, rel_period_2
  for (i in -2:2) {
    col_name <- paste0("rel_period_", i)
    expect_true(col_name %in% colnames(stacked_data),
                info = paste("Missing column:", col_name))
  }
})

test_that("stack_data works with different control types", {
  # Both controls (default)
  result_both <- stack_data("year_treated", "year", 2, 2, base_stagg, control_type = "both")
  expect_true(is.data.frame(result_both))

  # Never-treated only
  result_never <- stack_data("year_treated", "year", 2, 2, base_stagg, control_type = "never-treated")
  expect_true(is.data.frame(result_never))

  # Not-yet-treated only
  result_notyet <- stack_data("year_treated", "year", 2, 2, base_stagg, control_type = "not-yet-treated")
  expect_true(is.data.frame(result_notyet))
})

test_that("stack_data with different window sizes", {
  result_small <- stack_data("year_treated", "year", 1, 1, base_stagg)
  result_large <- stack_data("year_treated", "year", 3, 3, base_stagg)

  # Larger window should produce more dummy columns
  small_dummies <- sum(grepl("^rel_period_", colnames(result_small)))
  large_dummies <- sum(grepl("^rel_period_", colnames(result_large)))
  expect_true(large_dummies >= small_dummies)
})
