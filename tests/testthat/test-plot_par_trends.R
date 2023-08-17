library(testthat)
library(causalverse)
library(dplyr)
library(ggplot2)

# Sample data for the tests
data <- expand.grid(entity = 1:100, time = 1:10) %>%
  arrange(entity, time) %>%
  mutate(
    treatment = ifelse(entity <= 50, "Treated", "Control"),
    outcome1 = 0.5 * time + rnorm(n(), 0, 2) + ifelse(treatment == "Treated", 0, 0), # no actual treatment effect
    outcome2 = 3 + 0.3 * time + rnorm(n(), 0, 1) + ifelse(treatment == "Treated", 0, 2) # treatment effect = 2
  )

# Test that plot_par_trends returns list of plots by default
test_that("plot_par_trends returns list of ggplot objects", {
  results <- plot_par_trends(data = data,
                             metrics_and_names = list(outcome1 = "Outcome 1", outcome2 = "Outcome 2"),
                             treatment_status_var = "treatment",
                             time_var = list(time = "Time"),
                             smoothing_method = "loess")
  
  expect_s3_class(results, "list")  
  expect_s3_class(results[[1]], "ggplot")
})

# Test that plot_par_trends returns a data.frame when specified
test_that("plot_par_trends returns data frame when output_format is data.frame", {
  results <- plot_par_trends(data = data,
                             metrics_and_names = list(outcome1 = "Outcome 1", outcome2 = "Outcome 2"),
                             treatment_status_var = "treatment",
                             time_var = list(time = "Time"),
                             output_format = "data.frame")
  expect_s3_class(results, "data.frame")
})
