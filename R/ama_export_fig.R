#' Function to export a figure with AMA theme
#'
#' This function applies a custom theme to a ggplot2 figure and exports it to a given path.
#' It exports both an archived version with the current date and a current version without a date.
#'
#' @param figure A ggplot2 object.
#' @param filename A character string specifying the filename without the extension.
#' @param filepath A character string specifying the directory to save the file.
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
ama_export_fig <- function(figure, filename, filepath) {
  
  # Get current date
  date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Check if "archive" directory exists in the specified export path, if not, create it
  archive_dir <- file.path(filepath, "archive")
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
  }
  
  # Export figure with current date to "archive" folder
  archive_filename <- file.path(archive_dir, paste0(filename, "_", date, ".pdf"))
  ggplot2::ggsave(filename = archive_filename, plot = figure, device = "pdf", dpi = 800)
  
  # Export figure without current date to the specified export path
  current_filename <- file.path(filepath, paste0(filename, ".pdf"))
  ggplot2::ggsave(filename = current_filename, plot = figure, device = "pdf", dpi = 800)
}
