library(testthat)
library(causalverse)  
library(synthdid)
library(dplyr)

# Test 1: Function returns a list
test_that("synthdid_se_placebo returns a vector of double", {
  setup <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated"
  ) |>
    dplyr::mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
    dplyr::mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar))) |>
    synthdid::panel.matrices(
      unit = "id",
      time = "year",
      outcome = "y",
      treatment = "treatvar"
    )
  
  estimate <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
  result <- synthdid_se_placebo(estimate, replications = 1000)
  
  expect_type(result[1], "double")
})
