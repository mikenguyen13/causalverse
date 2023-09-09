library(testthat)
library(PanelMatch)
library(ggplot2)

# Use dem dataset from PanelMatch package
data("dem", package = "PanelMatch")

# Sample PanelMatch results for testing purposes
PM.results.maha.4lag.5m <- PanelMatch::PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "wbcode2",
  treatment = "dem",
  refinement.method = "mahalanobis",
  data = dem,
  match.missing = TRUE,
  covs.formula = ~ y + tradewb,
  size.match = 5,
  qoi = "att",
  outcome.var = "y",
  lead = 0:4,
  forbid.treatment.reversal = FALSE,
  use.diagonal.variance.matrix = TRUE
)

PM.results.maha.4lag.10m <- PanelMatch::PanelMatch(
  lag = 4,
  time.id = "year",
  unit.id = "wbcode2",
  treatment = "dem",
  refinement.method = "mahalanobis",
  data = dem,
  match.missing = TRUE,
  covs.formula = ~ y + tradewb,
  size.match = 10,
  qoi = "att",
  outcome.var = "y",
  lead = 0:4,
  forbid.treatment.reversal = FALSE,
  use.diagonal.variance.matrix = TRUE
)

test_that("balance_scatter_custom returns ggplot object", {
  result <- balance_scatter_custom(
    matched_set_list = list(PM.results.maha.4lag.5m$att, PM.results.maha.4lag.10m$att),
    set.names = c("Maha 4 Lag 5 Matches", "Maha 4 Lag 10 Matches"),
    data = dem,
    covariates = c("y", "tradewb")
  )
  expect_s3_class(result, "ggplot")
})

test_that("Error is thrown with empty matched_set_list", {
  expect_error(balance_scatter_custom(matched_set_list = list(), data = dem, covariates = c("y", "tradewb")), "Please provide at least one matched.set object")
})
