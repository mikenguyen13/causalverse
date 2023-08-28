#' Function to export a figure with custom settings
#'
#' This function exports a ggplot2 figure to a given path.
#' It exports both an archived version with the current date and a current version without a date.
#' The function supports exporting to PDF and JPG formats.
#'
#' @param figure A ggplot2 object.
#' @param filename A character string specifying the filename without the extension.
#' @param filepath A character string specifying the directory to save the file.
#' @param width The width of the image in inches (default is 7 inches).
#' @param height The height of the image in inches (default is 7 inches).
#'
#' @import ggplot2
#' @import lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' test_plot <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()  # Create a ggplot2 plot
#' filename <- "sample_plot"  # Define a filename
#' filepath <- tempdir()  # Define a path using a temporary directory
#' ama_export_fig(test_plot, filename, filepath)  # Call the ama_export_fig function
#' }
ama_export_fig <- function(figure, filename, filepath, width = 7, height = 7) {
  
  # Get current date
  date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Check if "archive" directory exists in the specified export path, if not, create it
  archive_dir <- file.path(filepath, "archive")
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
  }
  
  # Define formats to export
  formats <- c("pdf", "jpg")
  
  for (format in formats) {
    # Export figure with current date to "archive" folder
    archive_filename <- file.path(archive_dir, paste0(filename, "_", date, ".", format))
    ggplot2::ggsave(filename = archive_filename, plot = figure, device = format, dpi = 800, width = width, height = height)
    
    # Export figure without current date to the specified export path
    current_filename <- file.path(filepath, paste0(filename, ".", format))
    ggplot2::ggsave(filename = current_filename, plot = figure, device = format, dpi = 800, width = width, height = height)
  }
}
