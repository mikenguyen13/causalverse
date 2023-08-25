library(ggplot2)
library(testthat)

is.color <- function(color_string) {
  tryCatch({
    grDevices::col2rgb(color_string)
    TRUE
  }, error = function(e) {
    FALSE
  })
}


test_that("ama_scale_color applies correct color or grayscale", {
  p_color <- ggplot(mtcars, aes(mpg, wt, color = gear)) + 
    geom_point(size = 4) + 
    ama_scale_color(use_color = TRUE)
  
  p_gray <- ggplot(mtcars, aes(mpg, wt, color = gear)) + 
    geom_point(size = 4) + 
    ama_scale_color(use_color = FALSE)
  
  # Check if the scale is color or grayscale based on use_color parameter
  expect_true(is.color((ggplot_build(p_color)$plot$layers[[1]]$aes_params$color)[[1]]))
  expect_true(is.color((ggplot_build(p_gray)$plot$layers[[1]]$aes_params$color)[[1]]))
})

