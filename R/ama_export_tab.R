#' Function to export a table with AMA style
#'
#' This function exports the provided table in both Excel (`.xlsx`) and LaTeX (`.tex`) formats.
#' The table is archived with the current date in the filename for the Excel version, while
#' the LaTeX version is saved with just the specified filename.
#'
#' @param table A data frame or matrix.
#' @param filename A character string specifying the filename without the extension.
#' @param filepath A character string specifying the directory to save the file.
#' @param caption A character string specifying the caption for the table.
#' @param size Latex size including "tiny", or "small"
#' @import xtable
#' @import rio
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)  # Load the mtcars dataset
#' ama_export_tab(mtcars[1:5, 1:5], "sample_table", tempdir(), "Sample Caption for mtcars")
#' }
ama_export_tab <- function(table, filename, filepath, caption = NULL, size = "small") {
  # Get current date
  date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Create LaTeX table using xtable
  xtable_obj <- xtable::xtable(table, caption = caption, label = paste0("table:", filename))
  
  # Custom sanitize function to bold column names
  sanitize_custom <- function(text){
    return(ifelse(text %in% colnames(table),
                  paste("\\textbf{", text, "}", sep = ""),
                  text))
  }
  
  # If the tex file exists, delete it (i.e., to overwrite the file)
  if (file.exists(file.path(filepath, paste0(filename, ".tex")))){
    file.remove(file.path(filepath, paste0(filename, ".tex")))
  }
  
  # Check if "archive" directory exists, if not, create it
  if (!dir.exists(file.path(filepath, "archive"))) {
    dir.create(file.path(filepath, "archive"))
  }
  
  # Export table with current date to "archive" folder as Excel and LaTeX file
  rio::export(table, file.path(filepath ,"archive", paste0(filename, "_", date, ".xlsx")))
  
  # Export table without current date to current folder as Excel and LaTeX file
  rio::export(table, file.path(filepath , paste0(filename, ".xlsx")))
  
  # Export LaTeX table without current date to current folder as LaTeX file
  writeLines(capture.output(
    print(
      xtable_obj,
      type = "latex",
      caption.placement = "top",
      include.rownames = FALSE,
      size = size,
      sanitize.text.function = sanitize_custom
    )
  ), file.path(filepath, paste0(filename, ".tex")))
  
  # Export LaTeX table with current date to "archive" folder as LaTeX file
  writeLines(capture.output(print(xtable_obj, type = "latex", caption.placement = "top",include.rownames=FALSE, sanitize.text.function=sanitize_custom)), file.path(filepath, "archive", paste0(filename, "_", date, ".tex")))
}
