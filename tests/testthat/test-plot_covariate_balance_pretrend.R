library(testthat)
library(ggplot2) # for is.ggplot()


test_that("Function returns ggplot object with correct data", {
  
  # Create sample data
  balance_data_sample <- matrix(rnorm(20), nrow = 5, dimnames = list(paste0("t_", 1:5), NULL))
  
  # Run function
  p <- plot_covariate_balance_pretrend(balance_data_sample)
  
  # Check that the output is a ggplot object
  expect_true(is.ggplot(p))
})

