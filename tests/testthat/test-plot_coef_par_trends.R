# Load necessary libraries
library(fixest)
library(causalverse)
library(testthat)

# Test data
data("base_did")

# Test cases
test_that("Function works for coefplot type and combined plot", {
  result <- plot_coef_par_trends(
    data = base_did,
    dependent_vars = c(y = "Outcome 1", x1 = "Outcome 2"),
    time_var = "period",
    unit_treatment_status = "treat",
    unit_id_var = "id",
    plot_type = "coefplot",
    combined_plot = TRUE,
    plot_args = list(main = "Interaction coefficients Plot"),
    legend_title = "Metrics",
    legend_position = "bottomleft"
  )
  expect_true(is.list(result))
})

test_that("Function works for coefplot type and individual plots", {
  result <- plot_coef_par_trends(
    data = base_did,
    dependent_vars = c(y = "Outcome 1", x1 = "Outcome 2"),
    time_var = "period",
    unit_treatment_status = "treat",
    unit_id_var = "id",
    plot_type = "coefplot",
    combined_plot = FALSE
  )
  expect_true(is.list(result))
  expect_equal(length(result), length(c(y = "Outcome 1", x1 = "Outcome 2")))
})

test_that("Error is raised for invalid plot type", {
  expect_error(
    plot_coef_par_trends(
      data = base_did,
      dependent_vars = c(y = "Outcome 1", x1 = "Outcome 2"),
      time_var = "period",
      unit_treatment_status = "treat",
      unit_id_var = "id",
      plot_type = "invalid_plot_type"
    ),
    "plot_type should be either 'coefplot' or 'iplot'"
  )
})