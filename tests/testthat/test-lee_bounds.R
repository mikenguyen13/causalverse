library(testthat)
library(MedBounds) # Assuming MedBounds provides the necessary data/functions
library(dplyr)

# Sample data for testing
set.seed(123) # Set seed for reproducibility
example_data <- data.frame(
  d = sample(0:1, 100, replace = TRUE),
  m = rnorm(100),
  y = rnorm(100),
  cluster = sample(1:10, 100, replace = TRUE)
)


# Test case: With c_at_ratio specified
test_that("lee_bounds with c_at_ratio specified returns correct term", {
  result <- lee_bounds(df = example_data, d = "d", m = "m", y = "y", c_at_ratio = 0.5, numdraws = 10)
  
  # Check the term column for "Point estimate" since c_at_ratio is specified
  expect_true("Point estimate" %in% result$term)
})

