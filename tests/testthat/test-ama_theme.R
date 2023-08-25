library(ggplot2)
library(testthat)

test_that("ama_theme applies the correct theme settings with Arial", {
  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ama_theme(base_family = "sans")
  theme_elements <- p$theme$text$family
  
  expect_equal(theme_elements, "sans")
  
})

test_that("ama_theme applies the correct theme settings with Times New Roman", {
  p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ama_theme(base_family = "serif")
  theme_elements <- p$theme$text$family
  
  expect_equal(theme_elements, "serif")
  
})
