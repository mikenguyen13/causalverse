# Load the necessary packages
library(testthat)

# Define the test
test_that("nice_tab correctly rounds numeric columns", {
  # Create a sample data frame for testing
  data <- data.frame(
    a = c(1.23456, 2.34567, 3.45678),
    b = c(9.87654, 8.76543, 7.65432),
    c = letters[1:3],
    d = c(0.123456789, 0.987654321, 0.567891234)
  )
  
  # Apply the nice_tab function with 2 decimal places
  result <- nice_tab(data, digit_decimal = 2)
  
  # Check that numeric columns are rounded to 2 decimal places
  expect_equal(result$a, c(1.23, 2.35, 3.46))
  expect_equal(result$b, c(9.88, 8.77, 7.65))
  expect_equal(result$d, c(0.12, 0.99, 0.57))
  
  # Check that non-numeric columns remain unchanged
  expect_identical(result$c, data$c)
})

# Define an additional test to check for default digit_decimal value (2)
test_that("nice_tab defaults to 2 decimal places", {
  # Create a sample data frame for testing
  data <- data.frame(
    a = c(1.23456, 2.34567, 3.45678),
    b = c(9.87654, 8.76543, 7.65432)
  )
  
  # Apply the nice_tab function without specifying digit_decimal
  result <- nice_tab(data)
  
  # Check that numeric columns are rounded to 2 decimal places
  expect_equal(result$a, c(1.23, 2.35, 3.46))
  expect_equal(result$b, c(9.88, 8.77, 7.65))
})