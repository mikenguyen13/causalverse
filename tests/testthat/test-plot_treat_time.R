library(testthat)

# Test dataset
test_data <- data.frame(
  time_period = 1:10,
  treatment_random = sample(1:10, 10, replace = TRUE)
)

test_that("plot_treat_time produces expected output", {
  
  # Call the function
  p <- plot_treat_time(data = test_data, time_var = time_period, unit_treat = treatment_random)
  
  # Expect the output to be a ggplot object
  expect_s3_class(p, "ggplot")
  
})