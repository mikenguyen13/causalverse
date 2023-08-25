library(ggplot2)
library(testthat)

test_that("ama_labs applies correct label formatting", {
  p <- ggplot(mtcars, aes(mpg, wt)) + 
    geom_point() + 
    ama_labs(title = "this is a test title",
             x = "x axis label",
             y = "y axis label",
             fill = "fill legend",
             color = "color legend")
  
  plot_labels <- ggplot_build(p)$plot$labels
  
  expect_equal(plot_labels$title, "this is a Test Title")
  expect_equal(plot_labels$x, "x Axis Label")
  expect_equal(plot_labels$y, "y Axis Label")
})
