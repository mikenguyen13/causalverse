library(testthat)
library(tidyverse)
library(causalverse)

test_that("plot_trends_across_group works without errors", {
  # Sample data
  data <- data.frame(year = rep(2001:2020, times = 2),
                     dependent_variable = rnorm(40, 50, 10),
                     group = rep(c("treated", "control"), each = 20),
                     industry = rep(c("Tech", "Healthcare"), each = 20))
  
  expect_silent(plot_trends_across_group(data, "year", "dependent_variable", "group", "industry"))
})

test_that("plot_trends_across_group fails with non-existent variables", {
  data <- data.frame(year = 2001:2020,
                     dependent_variable = rnorm(20, 50, 10))
  
  expect_error(plot_trends_across_group(data, "year", "nonexistent", "group", "industry"),
               "One or more of the specified variables do not exist in the dataset.")
})

