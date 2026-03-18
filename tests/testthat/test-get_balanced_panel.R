library(testthat)
library(causalverse)

test_that("function returns correct output for valid input", {
  result <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated"
  )

  expect_true(is.data.frame(result))
})

test_that("balanced panel only includes correct time window", {
  result <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated"
  )

  # Time window should be [5-2, 5+3] = [3, 8]
  expect_true(min(result$year) >= 3)
  expect_true(max(result$year) <= 8)
})

test_that("balanced panel contains treated and control units", {
  result <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated"
  )

  # Should have treated units (year_treated == 5) and controls (year_treated > 8)
  treated <- result[result$year_treated == 5, ]
  controls <- result[result$year_treated > 8, ]

  expect_true(nrow(treated) > 0)
  expect_true(nrow(controls) > 0)
})

test_that("balanced panel without filtering retains more units", {
  result_filtered <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    filter_units = TRUE
  )

  result_unfiltered <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 5,
    lags = 2,
    leads = 3,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    filter_units = FALSE
  )

  expect_true(nrow(result_unfiltered) >= nrow(result_filtered))
})

test_that("different adoption cohorts produce different panels", {
  result5 <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 5,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated"
  )

  result6 <- get_balanced_panel(
    data = fixest::base_stagg,
    adoption_cohort = 6,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated"
  )

  # Time windows should differ
  expect_true(min(result5$year) != min(result6$year) ||
              max(result5$year) != max(result6$year))
})
