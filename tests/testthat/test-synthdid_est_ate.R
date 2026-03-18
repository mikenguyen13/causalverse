library(testthat)
library(causalverse)

data <- fixest::base_stagg |>
  dplyr::mutate(treatvar = dplyr::if_else(time_to_treatment >= 0, 1, 0)) |>
  dplyr::mutate(treatvar = as.integer(dplyr::if_else(year_treated > (5 + 2), 0, treatvar)))

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
  )
  expect_type(result, "list")
})

test_that("synthdid_est_ate returns all expected components", {
  result <- synthdid_est_ate(
    data = data,
    adoption_cohorts = 5:7,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )

  expected_names <- c("TE_mean", "SE_mean", "TE_mean_lower", "TE_mean_upper",
                      "TE_mean_w", "SE_mean_w", "TE_mean_w_lower", "TE_mean_w_upper",
                      "Ntr", "Nco", "TE", "SE", "y_obs", "y_pred", "time", "col_names")
  expect_true(all(expected_names %in% names(result)))
})

test_that("synthdid_est_ate handles incorrect data types", {
  expect_error(synthdid_est_ate(
    data = "not a dataframe",
    adoption_cohorts = 5:7,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  ))
})

test_that("synthdid_est_ate CI is consistent with estimates and SE", {
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
    conf_level = 0.95
  )

  # Lower CI should be below the estimate
  expect_true(all(result$TE_mean_lower <= result$TE_mean))
  # Upper CI should be above the estimate
  expect_true(all(result$TE_mean_upper >= result$TE_mean))
})

test_that("synthdid_est_ate time vector has correct length", {
  lags <- 2
  leads <- 2
  result <- synthdid_est_ate(
    data = data,
    adoption_cohorts = 5:7,
    lags = lags,
    leads = leads,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )

  expect_equal(length(result$time), lags + leads + 1)
  expect_equal(result$time[1], -lags)
  expect_equal(result$time[length(result$time)], leads)
})

test_that("synthdid_est_ate number of treated/control units are positive", {
  result <- synthdid_est_ate(
    data = data,
    adoption_cohorts = 5:7,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )

  expect_true(all(result$Ntr > 0))
  expect_true(all(result$Nco > 0))
})

test_that("synthdid_est_ate with single cohort works", {
  result <- synthdid_est_ate(
    data = data,
    adoption_cohorts = 5,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y"
  )

  expect_type(result, "list")
  expect_equal(length(result$Ntr), 1)
})

test_that("synthdid_est_ate works with did method", {
  result <- synthdid_est_ate(
    data = data,
    adoption_cohorts = 5:6,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    outcome_var = "y",
    method = "did"
  )

  expect_type(result, "list")
  expect_true(is.numeric(result$TE_mean))
})
