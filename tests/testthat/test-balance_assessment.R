library(testthat)

test_that("balance_assessment returns expected SUR and Hotelling results", {
  set.seed(123)
  data = mtcars %>% 
    dplyr::select(mpg, cyl, disp, hp, wt) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(treatment = sample(c(0,1), 1, replace = TRUE)) %>% 
    dplyr::ungroup()
  
  results <- balance_assessment(data, "treatment", "mpg", "cyl")
  
  # Check that results$SUR has the class 'summary.systemfit'
  expect_s3_class(results$SUR, "summary.systemfit")
  
  # Check that results$Hotelling has the class 'hotelling.test'
  expect_s3_class(results$Hotelling, "hotelling.test")
})
