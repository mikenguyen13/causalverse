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
  dplyr::mutate(treatvar = dplyr::if_else(time_to_treatment >= 0, 1, 0)) |>
  dplyr::mutate(treatvar = as.integer(dplyr::if_else(year_treated > (5 + 2), 0, treatvar)))

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

test_that("Output structure has all expected components", {
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

  expected_names <- c("est", "se", "y_pred", "y_obs", "lambda.synth", "Ntr", "Nco")
  expect_true(all(expected_names %in% names(result)))
})

test_that("synthdid_est returns numeric estimates", {
  result <- synthdid_est(
    data = mock_data,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )

  expect_true(is.numeric(result$est))
  expect_true(is.numeric(result$se))
  expect_true(result$Ntr > 0)
  expect_true(result$Nco > 0)
})

test_that("synthdid_est works with 'did' method", {
  result <- synthdid_est(
    data = mock_data,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y",
    method = "did"
  )

  expect_type(result, "list")
  expect_true(is.numeric(result$est))
})

test_that("synthdid_est works with 'sc' method", {
  result <- synthdid_est(
    data = mock_data,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y",
    method = "sc"
  )

  expect_type(result, "list")
  expect_true(is.numeric(result$est))
})

test_that("synthdid_est estimate length matches lags + leads + 1 plus cumulative", {
  lags <- 2
  leads <- 3
  result <- synthdid_est(
    data = mock_data,
    adoption_cohort = 5,
    lags = lags,
    leads = leads,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )

  # est should have lags + leads + 1 periods + leads + 1 cumulative ATEs
  expected_length <- (lags + leads + 1) + (leads + 1)
  expect_equal(length(result$est), expected_length)
})
