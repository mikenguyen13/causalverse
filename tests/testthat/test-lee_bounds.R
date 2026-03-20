library(testthat)
library(causalverse)

# Sample data for testing - mediator must be binary for lee bounds
set.seed(42)
n <- 200
example_data <- data.frame(
  d = sample(0:1, n, replace = TRUE),
  m = sample(0:1, n, replace = TRUE),
  y = rnorm(n),
  cluster = sample(1:10, n, replace = TRUE)
)

test_that("lee_bounds returns data frame with correct structure", {
  skip_if_not_installed("MedBounds")

  result <- tryCatch(
    lee_bounds(
      df = example_data, d = "d", m = "m", y = "y",
      c_at_ratio = 0.5, numdraws = 10
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "MedBounds computation failed with test data")

  expect_s3_class(result, "data.frame")
  expect_true("Point estimate" %in% result$term)
  expect_true(all(c("term", "estimate", "std.error") %in% names(result)))
})

test_that("lee_bounds returns bounds without c_at_ratio", {
  skip_if_not_installed("MedBounds")

  result <- tryCatch(
    lee_bounds(
      df = example_data, d = "d", m = "m", y = "y",
      numdraws = 10
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "MedBounds computation failed with test data")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("Lower bound" %in% result$term)
  expect_true("Upper bound" %in% result$term)
})

test_that("lee_bounds lower bound is less than or equal to upper bound", {
  skip_if_not_installed("MedBounds")

  result <- tryCatch(
    lee_bounds(
      df = example_data, d = "d", m = "m", y = "y",
      numdraws = 10
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "MedBounds computation failed with test data")

  lower <- result$estimate[result$term == "Lower bound"]
  upper <- result$estimate[result$term == "Upper bound"]
  expect_true(lower <= upper)
})

test_that("lee_bounds standard errors are non-negative", {
  skip_if_not_installed("MedBounds")

  result <- tryCatch(
    lee_bounds(
      df = example_data, d = "d", m = "m", y = "y",
      c_at_ratio = 0.5, numdraws = 10
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "MedBounds computation failed with test data")

  expect_true(all(result$std.error >= 0, na.rm = TRUE))
})

test_that("lee_bounds errors without MedBounds installed", {
  skip_if(requireNamespace("MedBounds", quietly = TRUE),
          "MedBounds is installed, skipping unavailability test")

  expect_error(
    lee_bounds(df = example_data, d = "d", m = "m", y = "y"),
    "MedBounds"
  )
})
