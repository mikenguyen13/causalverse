library(testthat)
library(ggplot2)
library(dplyr)

test_that("plot_density_by_treatment produces correct number of plots", {
  data(mtcars)
  data <- mtcars %>% 
    dplyr::select(mpg, cyl) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(treatment = sample(c(0,1), 1, replace = TRUE)) %>% 
    dplyr::ungroup()
  
  plots <- plot_density_by_treatment(
    data = data,
    var_map = list("mpg" = "Var 1",
                   "cyl" = "Var 2"),
    treatment_var = c("treatment" = "Treatment Name\nin Legend")
  )
  
  expect_equal(length(plots), 2)
  expect_s3_class(plots[["Var 1"]], "ggplot")
  expect_s3_class(plots[["Var 2"]], "ggplot")
})

# Add more tests as necessary
