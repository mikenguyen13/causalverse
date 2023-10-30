library(testthat)
library(tidyverse)
library(causalverse)
library(fixest)

test_that("stack_data function processes correctly", {
  
  data(base_stagg)
  
  # Test that the function runs without error
  stacked_data <- stack_data("year_treated", "year", 5, 5, base_stagg)
  expect_true(is.data.frame(stacked_data))
  
  # Test for the presence of df and rel_period columns
  expect_true("df" %in% colnames(stacked_data))
  expect_true("rel_period" %in% colnames(stacked_data))
  
  # Test that the control group is not present in the stacked data
  expect_false(10000 %in% stacked_data$df)
  
})

