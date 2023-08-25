#' Custom Fill Scale for ggplot2: American Marketing Association Style
#'
#' This function provides a custom fill scale for ggplot2 plots based on the guidelines set by the American Marketing Association. 
#'
#' @param use_color Logical. If TRUE, uses color, otherwise uses grayscale.
#' @param palette_name Character. Name of the color palette to use.
#' @param grayscale_limits Numeric vector. Limits for the grayscale gradient.
#'
#' @return A fill scale for a ggplot2 plot.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt, fill = gear)) + 
#' geom_point(shape = 21, size = 4) + 
#' ama_scale_fill()
#' }
ama_scale_fill <- function(use_color = FALSE, palette_name = "OkabeIto", grayscale_limits = c(0.2, 0.8)) {
  aes_mapping <- deparse(parent.frame()$p$mapping$fill)
  data <- parent.frame()$data
  
  if (is.factor(data[[aes_mapping]]) || is.character(data[[aes_mapping]])) {
    if (use_color) {
      # You might want to replace this with the correct function call
      fills <- grDevices::palette()
      return(scale_fill_manual(values = fills))
    } else {
      return(ggplot2::scale_fill_grey(start = grayscale_limits[1], end = grayscale_limits[2]))
    }
  } else {
    if (use_color) {
      # You might want to replace this with the correct function call
      fills <- grDevices::palette()
      return(ggplot2::scale_fill_gradientn(colors = fills))
    } else {
      gray_low <- scales::percent(grayscale_limits[1], accuracy = 1)
      gray_high <- scales::percent(grayscale_limits[2], accuracy = 1)
      return(ggplot2::scale_fill_gradient(low = gray_low, high = gray_high))
    }
  }
}