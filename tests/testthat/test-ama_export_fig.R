library(testthat)
library(ggplot2)

# Assuming the ama_export_fig function and any other necessary functions like amatheme() are loaded.

test_that("ama_export_fig exports to PDF correctly", {
  # Create a simple test plot
  test_plot <- ggplot(mpg, aes(x=displ, y=hwy)) +
    geom_point()
  
  # Define a temporary directory to save our tests
  temp_path <- tempdir()
  
  # Call the function to save the plot
  ama_export_fig(test_plot, "test_figure", temp_path)
  
  # Check that files are created
  archive_file_path <- file.path(temp_path, "archive", paste0("test_figure_", format(Sys.Date(), "%Y-%m-%d"), ".pdf"))
  current_file_path <- file.path(temp_path, "test_figure.pdf")
  
  expect_true(file.exists(archive_file_path))
  expect_true(file.exists(current_file_path))
})