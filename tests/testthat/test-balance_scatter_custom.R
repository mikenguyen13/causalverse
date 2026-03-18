library(testthat)
library(PanelMatch)
library(ggplot2)

# Use dem dataset from PanelMatch package
data("dem", package = "PanelMatch")

# Create PanelData object (required in PanelMatch >= 3.1.0)
pd <- PanelData(panel.data = dem, unit.id = "wbcode2",
                time.id = "year", treatment = "dem", outcome = "y")

# PanelMatch results for testing
PM.results.5m <- PanelMatch(
  panel.data = pd,
  lag = 4,
  refinement.method = "mahalanobis",
  match.missing = TRUE,
  covs.formula = ~ y + tradewb,
  size.match = 5,
  qoi = "att",
  lead = 0:4,
  forbid.treatment.reversal = FALSE,
  use.diagonal.variance.matrix = TRUE
)

PM.results.10m <- PanelMatch(
  panel.data = pd,
  lag = 4,
  refinement.method = "mahalanobis",
  match.missing = TRUE,
  covs.formula = ~ y + tradewb,
  size.match = 10,
  qoi = "att",
  lead = 0:4,
  forbid.treatment.reversal = FALSE,
  use.diagonal.variance.matrix = TRUE
)

test_that("balance_scatter_custom returns ggplot object", {
  result <- balance_scatter_custom(
    pm_result_list = list(PM.results.5m, PM.results.10m),
    panel.data = pd,
    set.names = c("Maha 4 Lag 5 Matches", "Maha 4 Lag 10 Matches"),
    covariates = c("y", "tradewb")
  )
  expect_s3_class(result, "ggplot")
})

test_that("balance_scatter_custom works with single result", {
  result <- balance_scatter_custom(
    pm_result_list = list(PM.results.5m),
    panel.data = pd,
    covariates = c("y", "tradewb")
  )
  expect_s3_class(result, "ggplot")
})

test_that("Error is thrown with empty pm_result_list", {
  expect_error(
    balance_scatter_custom(
      pm_result_list = list(),
      panel.data = pd,
      covariates = c("y", "tradewb")
    ),
    "Please provide at least one"
  )
})

test_that("Error is thrown with mismatched set.names length", {
  expect_error(
    balance_scatter_custom(
      pm_result_list = list(PM.results.5m, PM.results.10m),
      panel.data = pd,
      set.names = c("Only One Name"),
      covariates = c("y", "tradewb")
    ),
    "set.names should match"
  )
})

test_that("balance_scatter_custom legend can be hidden", {
  result <- balance_scatter_custom(
    pm_result_list = list(PM.results.5m),
    panel.data = pd,
    covariates = c("y", "tradewb"),
    show.legend = FALSE
  )
  expect_s3_class(result, "ggplot")
})
