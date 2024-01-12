library(testthat)
library(synthdid)

# Test with a known input
test_that("process_panel_estimate returns correct format and values", {
  # Create a mock result similar to what panel_estimate() would return
  mock_result <- list(
    method1 = list(estimate = 1.5, std.error = 0.1),
    method2 = list(estimate = 2.0, std.error = 0.2)
  )
  names(mock_result) <- c("method1", "method2")
  
  # Process the mock result
  processed_result <- process_panel_estimate(mock_result)
  
  # Check if the result is a data frame
  expect_true(is.data.frame(processed_result))
  
  # Check the number of rows and columns
  expect_equal(nrow(processed_result), 2)
  expect_equal(ncol(processed_result), 3)
  
  # Check the column names
  expect_equal(colnames(processed_result), c("Method", "Estimate", "SE"))
  
  # Check the values
  expect_equal(processed_result$Method, c("METHOD1", "METHOD2"))
  expect_equal(processed_result$Estimate, c(1.5, 2.0))
  expect_equal(processed_result$SE, c(0.1, 0.2))
})

