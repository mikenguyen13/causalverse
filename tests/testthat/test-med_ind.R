library(testthat)
library(causalverse) 

test_that("med_ind returns correct types", {
  set.seed(1) # Setting seed for reproducibility
  result <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01)
  
  # Check if the result is a list
  expect_true(is.list(result))
  
  # Check if the list contains all the expected elements
  expect_true(all(c("lower_quantile", "upper_quantile", "raw_data", "plot") %in% names(result)))
  
  # Check if the elements have the correct types
  expect_true(is.numeric(result$lower_quantile))
  expect_true(is.numeric(result$upper_quantile))
  expect_true(is.numeric(result$raw_data))
  expect_true(is.ggplot(result$plot))
})

test_that("med_ind returns expected values with set seed", {
  set.seed(1) # Setting seed for reproducibility
  result <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01)
  
  # Check if the quantiles are within expected range (specific values would depend on the exact simulation)
  expect_true(result$lower_quantile > 0)
  expect_true(result$upper_quantile > result$lower_quantile)
  
  # Optionally, check the length of the raw_data which should be equal to the number of iterations
  expect_equal(length(result$raw_data), 20000)
})

