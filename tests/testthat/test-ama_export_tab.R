library(testthat)
library(xtable)
library(rio)

# Assuming ama_export_tab and any other necessary functions are loaded.

test_that("ama_export_tab exports tables correctly", {
  # Create a test data frame
  test_df <- data.frame(A = 1:3, B = c("x", "y", "z"))
  
  # Define a temporary directory to save our tests
  temp_path <- tempdir()
  
  # Call the function to save the table
  ama_export_tab(test_df, "test_table", temp_path, "Test Table Caption")
  
  # Check that files are created
  archive_file_xlsx_path <- file.path(temp_path, "archive", paste0("test_table_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
  current_file_xlsx_path <- file.path(temp_path, "test_table.xlsx")
  current_file_tex_path <- file.path(temp_path, "test_table.tex")
  
  expect_true(file.exists(archive_file_xlsx_path))
  expect_true(file.exists(current_file_xlsx_path))
  expect_true(file.exists(current_file_tex_path))
})