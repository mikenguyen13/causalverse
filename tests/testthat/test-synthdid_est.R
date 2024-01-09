library(testthat)
library(synthdid)
library(causalverse)

mock_data <- get_balanced_panel(
  data = fixest::base_stagg,
  adoption_cohort = 5,
  lags = 2,
  leads = 3,
  time_var = "year",
  unit_id_var = "id",
  treated_period_var = "year_treated"
) |>
  dplyr::mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
  
  dplyr::mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar)))

# Example test: Check if the function returns a list
test_that("synthdid_est returns a list", {
  
  result <- synthdid_est(
    data = mock_data,
    adoption_cohort = 5,
    subgroup = NULL,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )
  
  expect_type(result, "list")
})

# Example test: Check the structure of the output
test_that("Output structure is correct", {
  result <- synthdid_est(
    data = mock_data,
    adoption_cohort = 5,
    subgroup = NULL,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )
  
  # Check if all expected list elements are present
  expect_true(all(c("est", "se", "y_pred", "y_obs", "lambda.synth", "Ntr", "Nco") %in% names(result)))
})

