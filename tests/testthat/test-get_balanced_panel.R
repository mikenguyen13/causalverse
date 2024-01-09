library(testthat)
library(causalverse) 

# Sample data for testing
test_data <- data.frame(
  id = rep(1:5, each = 3),
  year = rep(1:3, times = 5),
  year_treated = c(2, 2, 2, 3, 3, 3, 1, 1, 1, 4, 4, 4, 5, 5, 5)
)

test_that("function returns correct output for valid input", {
  result <- get_balanced_panel(data = fixest::base_stagg,
                               adoption_cohort = 5,
                               lags = 2,
                               leads = 3,
                               time_var = "year",
                               unit_id_var = "id",
                               treated_period_var = "year_treated")
  
  expect_true(is.data.frame(result))
})
