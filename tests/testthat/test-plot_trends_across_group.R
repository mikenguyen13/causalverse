library(testthat)
library(causalverse)

sample_data <- data.frame(
  year = rep(2001:2005, each = 4),
  dependent_variable = rnorm(20, mean = 50, sd = 10),
  group = rep(c("treated", "control"), times = 10),
  industry = rep(c("Tech", "Healthcare"), each = 10),
  se_col = abs(rnorm(20, mean = 2, sd = 1))
)

test_that("plot_trends_across_group returns ggplot object", {
  p <- plot_trends_across_group(
    data = sample_data,
    x_var = "year",
    y_var = "dependent_variable",
    grouping_var = "group",
    facet_var = "industry",
    title = "Test Plot"
  )

  expect_s3_class(p, "gg")
})

test_that("plot_trends_across_group errors on non-existent variables", {
  expect_error(
    plot_trends_across_group(
      data = sample_data,
      x_var = "nonexistent",
      y_var = "dependent_variable",
      grouping_var = "group",
      facet_var = "industry"
    ),
    "One or more of the specified variables do not exist"
  )
})

test_that("plot_trends_across_group errors on non-numeric y_var", {
  bad_data <- sample_data
  bad_data$dependent_variable <- as.character(bad_data$dependent_variable)

  expect_error(
    plot_trends_across_group(
      data = bad_data,
      x_var = "year",
      y_var = "dependent_variable",
      grouping_var = "group",
      facet_var = "industry"
    ),
    "must be numeric"
  )
})

test_that("plot_trends_across_group works with standard errors", {
  p <- plot_trends_across_group(
    data = sample_data,
    x_var = "year",
    y_var = "dependent_variable",
    grouping_var = "group",
    facet_var = "industry",
    se = "se_col"
  )

  expect_s3_class(p, "gg")
})

test_that("plot_trends_across_group errors on non-existent se variable", {
  expect_error(
    plot_trends_across_group(
      data = sample_data,
      x_var = "year",
      y_var = "dependent_variable",
      grouping_var = "group",
      facet_var = "industry",
      se = "nonexistent_se"
    ),
    "does not exist"
  )
})

test_that("plot_trends_across_group legend can be removed", {
  p <- plot_trends_across_group(
    data = sample_data,
    x_var = "year",
    y_var = "dependent_variable",
    grouping_var = "group",
    facet_var = "industry",
    include_legend = FALSE
  )

  expect_s3_class(p, "gg")
})
