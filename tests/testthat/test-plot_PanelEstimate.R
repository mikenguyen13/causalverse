library(testthat)
library(PanelMatch)
library(causalverse)

# Create PanelData object
data("dem", package = "PanelMatch")
pd <- PanelData(panel.data = dem, unit.id = "wbcode2",
                time.id = "year", treatment = "dem", outcome = "y")

test_that("plot_panel_estimate returns a ggplot object", {
  PM.results.ps.weight <- PanelMatch(
    panel.data = pd,
    lag = 4,
    refinement.method = "ps.weight",
    match.missing = FALSE,
    listwise.delete = TRUE,
    covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
    size.match = 5,
    qoi = "att",
    lead = 0:4,
    forbid.treatment.reversal = FALSE
  )

  PE.results <- PanelEstimate(
    sets = PM.results.ps.weight,
    panel.data = pd,
    se.method = "bootstrap",
    number.iterations = 100,
    confidence.level = .95
  )

  plot_result <- plot_panel_estimate(PE.results)
  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_panel_estimate works with custom labels", {
  PM.results <- PanelMatch(
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

  PE.results <- PanelEstimate(
    sets = PM.results,
    panel.data = pd,
    se.method = "bootstrap",
    number.iterations = 100,
    confidence.level = .95
  )

  plot_result <- plot_panel_estimate(
    PE.results,
    ylab = "Custom Y",
    xlab = "Custom X",
    main = "Custom Title"
  )
  expect_s3_class(plot_result, "ggplot")
})

test_that("plot_panel_estimate works with custom ylim", {
  PM.results <- PanelMatch(
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

  PE.results <- PanelEstimate(
    sets = PM.results,
    panel.data = pd,
    se.method = "bootstrap",
    number.iterations = 100,
    confidence.level = .95
  )

  plot_result <- plot_panel_estimate(PE.results, ylim = c(-10, 10))
  expect_s3_class(plot_result, "ggplot")
})
