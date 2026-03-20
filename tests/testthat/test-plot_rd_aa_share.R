library(testthat)
library(causalverse)

test_that("plot_rd_aa_share returns ggplot when given rdbounds output", {
  skip_if_not_installed("rdbounds")

  set.seed(1)
  data <- rdbounds::rdbounds_sampledata(1000, covs = FALSE)
  rdbounds_est <- suppressWarnings(rdbounds::rdbounds(
    y = data$y,
    x = data$x,
    treatment = data$treatment,
    c = 0,
    discrete_x = FALSE,
    discrete_y = FALSE,
    bwsx = c(.2, .5),
    bwy = 1,
    kernel = "epanechnikov",
    orders = 1,
    evaluation_ys = seq(from = 0, to = 15, by = 3),
    refinement_A = TRUE,
    refinement_B = TRUE,
    right_effects = TRUE,
    potential_taus = c(.05, .1, .2),
    yextremes = c(0, 15),
    num_bootstraps = 3
  ))

  p <- plot_rd_aa_share(rdbounds_est)
  expect_s3_class(p, "gg")
})
