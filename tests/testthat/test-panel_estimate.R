library(testthat)
library(synthdid)

test_that("panel_estimate works with default parameters", {
  data('california_prop99')
  setup <- panel.matrices(california_prop99)

  results <- panel_estimate(setup)

  expect_true(is.list(results))
  # Default excludes 'mc', so should have 6 estimators
  expect_equal(length(results), 6)
})

test_that("panel_estimate works with specific estimators", {
  data('california_prop99')
  setup <- panel.matrices(california_prop99)

  results <- panel_estimate(setup, selected_estimators = c("did", "sc"))

  expect_equal(length(results), 2)
  expect_true(all(c("did", "sc") %in% names(results)))
})

test_that("panel_estimate returns correct structure per estimator", {
  data('california_prop99')
  setup <- panel.matrices(california_prop99)

  results <- panel_estimate(setup, selected_estimators = c("synthdid"))

  expect_true("synthdid" %in% names(results))
  expect_true("estimate" %in% names(results$synthdid))
  expect_true("std.error" %in% names(results$synthdid))
})

test_that("panel_estimate standard errors are non-negative", {
  data('california_prop99')
  setup <- panel.matrices(california_prop99)

  results <- panel_estimate(setup, selected_estimators = c("did", "sc"))

  for (name in names(results)) {
    expect_true(results[[name]]$std.error >= 0,
                info = paste("SE for", name, "should be non-negative"))
  }
})

test_that("panel_estimate single estimator works", {
  data('california_prop99')
  setup <- panel.matrices(california_prop99)

  results <- panel_estimate(setup, selected_estimators = "did")

  expect_equal(length(results), 1)
  expect_true("did" %in% names(results))
})

test_that("panel_estimate is reproducible with same seed", {
  data('california_prop99')
  setup <- panel.matrices(california_prop99)

  results1 <- panel_estimate(setup, selected_estimators = "did", seed = 42)
  results2 <- panel_estimate(setup, selected_estimators = "did", seed = 42)

  expect_equal(results1$did$std.error, results2$did$std.error)
})
