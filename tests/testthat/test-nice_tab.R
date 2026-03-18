library(testthat)

test_that("nice_tab correctly rounds numeric columns", {
  data <- data.frame(
    a = c(1.23456, 2.34567, 3.45678),
    b = c(9.87654, 8.76543, 7.65432),
    c = letters[1:3],
    d = c(0.123456789, 0.987654321, 0.567891234)
  )

  result <- nice_tab(data, digit_decimal = 2)

  expect_equal(result$a, c(1.23, 2.35, 3.46))
  expect_equal(result$b, c(9.88, 8.77, 7.65))
  expect_equal(result$d, c(0.12, 0.99, 0.57))
  expect_identical(result$c, data$c)
})

test_that("nice_tab defaults to 2 decimal places", {
  data <- data.frame(
    a = c(1.23456, 2.34567, 3.45678),
    b = c(9.87654, 8.76543, 7.65432)
  )

  result <- nice_tab(data)

  expect_equal(result$a, c(1.23, 2.35, 3.46))
  expect_equal(result$b, c(9.88, 8.77, 7.65))
})

test_that("nice_tab works with different decimal places", {
  data <- data.frame(x = c(1.23456789, 2.3456789))

  result0 <- nice_tab(data, digit_decimal = 0)
  expect_equal(result0$x, c(1, 2))

  result1 <- nice_tab(data, digit_decimal = 1)
  expect_equal(result1$x, c(1.2, 2.3))

  result4 <- nice_tab(data, digit_decimal = 4)
  expect_equal(result4$x, c(1.2346, 2.3457))
})

test_that("nice_tab handles data frame with only non-numeric columns", {
  data <- data.frame(a = letters[1:3], b = LETTERS[1:3])
  result <- nice_tab(data)

  expect_identical(result$a, data$a)
  expect_identical(result$b, data$b)
})

test_that("nice_tab handles single-column data frame", {
  data <- data.frame(x = c(1.111, 2.222, 3.333))
  result <- nice_tab(data, digit_decimal = 1)
  expect_equal(result$x, c(1.1, 2.2, 3.3))
})

test_that("nice_tab preserves data frame structure", {
  data <- data.frame(
    num = c(1.5, 2.5),
    char = c("a", "b"),
    int = c(1L, 2L),
    logi = c(TRUE, FALSE)
  )
  result <- nice_tab(data, digit_decimal = 0)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 4)
})

test_that("nice_tab handles NA values", {
  data <- data.frame(x = c(1.234, NA, 3.456))
  result <- nice_tab(data, digit_decimal = 1)
  expect_equal(result$x, c(1.2, NA, 3.5))
})

test_that("nice_tab handles negative numbers", {
  data <- data.frame(x = c(-1.2345, -9.8765))
  result <- nice_tab(data, digit_decimal = 2)
  expect_equal(result$x, c(-1.23, -9.88))
})
