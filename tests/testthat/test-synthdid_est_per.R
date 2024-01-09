library(testthat)
library(causalverse) 
library(tidyverse)
library(synthdid)

# Sample setup for testing
setup <- base_did |>
  mutate(
    id = as.factor(id),
    period = as.integer(period),
    y = as.double(y),
    post = as.integer(post)
  ) |>
  dplyr::mutate(treatment = as.integer(if_else(treat == 0, 0, post))) |>
  synthdid::panel.matrices(unit = "id", time = "period", outcome = "y", treatment = "treatment")

sdid <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)

# Test case 1: Function returns a list
test_that("synthdid_est_per returns a list", {
  result <- synthdid_est_per(setup$Y, setup$N0, setup$T0, weights = attr(sdid, 'weights'))
  expect_type(result, "list")
})

# Test case 2: Function returns expected components
test_that("synthdid_est_per returns all expected components", {
  result <- synthdid_est_per(setup$Y, setup$N0, setup$T0, weights = attr(sdid, 'weights'))
  expected_components <- c("est", "y_obs", "y_pred", "lambda.synth", "Ntr", "Nco")
  expect_true(all(expected_components %in% names(result)))
})
