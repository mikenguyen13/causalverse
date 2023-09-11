library(testthat)
library(PanelMatch)
library(ggplot2)

test_that("plot_PanelEstimate returns a ggplot object", {
  
  # Assumed data preparation... (You'd replace this with a small mock data or use the existing data if it's manageable)
  PM.results.ps.weight <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
                                     treatment = "dem", refinement.method = "ps.weight",
                                     data = dem, match.missing = FALSE, listwise.delete = TRUE,
                                     covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
                                     size.match = 5, qoi = "att", outcome.var = "y",
                                     lead = 0:4, forbid.treatment.reversal = FALSE)
  
  PE.results <- PanelEstimate(sets = PM.results.ps.weight, 
                              data = dem,
                              se.method = "bootstrap",
                              number.iterations = 1000,
                              confidence.level = .95)
  
  plot_result <- plot_PanelEstimate(PE.results)
  expect_s3_class(plot_result, "ggplot")
})
