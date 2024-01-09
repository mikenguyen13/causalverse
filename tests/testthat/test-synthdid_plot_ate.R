library(testthat)
library(ggplot2)
library(tidyverse)
library(causalverse)
library(tidyverse)
data <- fixest::base_stagg |>
  dplyr::mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
  dplyr::mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar)))
est <-
  synthdid_est_ate(
    data = data,
    adoption_cohorts = 5:7,
    lags = 2,
    leads = 2,
    time_var = "year",
    unit_id_var = "id",
    treated_period_var = "year_treated",
    treat_stat_var = "treatvar",
    pooled = FALSE,
    placebo = FALSE,
    outcome_var = "y"
  )

test_that("synthdid_plot returns a ggplot object", {
  plot <- synthdid_plot(est)
  expect_true(is.ggplot(plot))
})
