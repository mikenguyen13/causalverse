library(testthat)
library(causalverse)
library(tidyverse)

data <- fixest::base_stagg |>
  dplyr::mutate(treatvar = dplyr::if_else(time_to_treatment >= 0, 1, 0)) |>
  dplyr::mutate(treatvar = as.integer(dplyr::if_else(year_treated > (5 + 2), 0, treatvar)))

# Test Case 1: Test with valid inputs
test_that("synthdid_est_ate works with valid inputs", {
  result <- synthdid_est_ate(
    data = data,
    adoption_cohorts = 5:7,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y",
    placebo = FALSE,
    pooled = FALSE
    # Add other necessary parameters
  )
  expect_type(result, "list")
  # Add more expectations to check the structure and values of the output
})

# Test Case 2: Test for incorrect data types
test_that("synthdid_est_ate handles incorrect data types", {
  expect_error(synthdid_est_ate("not a dataframe", ...))
  
})

# Test Case 3: Test for missing parameters
test_that("synthdid_est_ate handles missing parameters", {
  expect_error(synthdid_est_ate(data = data))
  
})
