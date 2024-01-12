library(testthat)
library(synthdid)

# Test with default parameters
# test_that("panel_estimate works with default parameters", {
#   data('california_prop99')
#   setup = panel.matrices(california_prop99)
#   
#   results = panel_estimate(setup)
#   
#   # Check if the result is a list
#   expect_true(is.list(results))
#   
# })

# # Test with specific estimators
# test_that("panel_estimate works with specific estimators", {
#   data('california_prop99')
#   setup = panel.matrices(california_prop99)
#   
#   results = panel_estimate(setup, selected_estimators = c("did", "sc"))
#   
#   # Check if the result contains only the selected estimators
#   expect_equal(length(results), 2)
#   expect_true(all(c("did", "sc") %in% names(results)))
#   
# })
