#' Custom Label Formatting for ggplot2: American Marketing Association Style
#'
#' This function provides custom label formatting for ggplot2 based on the guidelines set by the American Marketing Association. 
#'
#' @param title Plot title.
#' @param subtitle Plot subtitle.
#' @param caption Plot caption.
#' @param x X-axis label.
#' @param y Y-axis label.
#' @param fill Fill legend title.
#' @param color Color legend title.
#' @param ... Additional arguments to be passed to `ggplot2::labs()`.
#'
#' @return Modified labels for a ggplot2 plot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + 
#' ama_labs(title = "Sample Plot") +
#' ama_theme()
#' }
ama_labs <-
  function(title = NULL,
           subtitle = NULL,
           caption = NULL,
           x = NULL,
           y = NULL,
           fill = NULL,
           color = NULL,
           ...) {
    headline_style <- function(string) {
      tools::toTitleCase(string)
    }
    
    sentence_style <- function(string) {
      paste0(toupper(substring(string, 1, 1)), tolower(substring(string, 2)))
    }
    
    ggplot2::labs(
      title = if (!is.null(title))
        headline_style(title)
      else
        NULL,
      subtitle = subtitle,
      caption = caption,
      x = if (!is.null(x))
        headline_style(x)
      else
        NULL,
      y = if (!is.null(y))
        headline_style(y)
      else
        NULL,
      fill = if (!is.null(fill))
        sentence_style(fill)
      else
        NULL,
      color = if (!is.null(color))
        sentence_style(color)
      else
        NULL,
      ...
    )
  }
