#' Custom Theme for ggplot2: American Marketing Association Style
#'
#' This function provides a custom theme for ggplot2 following the guidelines set by the American Marketing Association. 
#'
#' @param base_size Base font size.
#' @param base_family Font family. Use "sans" for Arial and "serif" for Times New Roman.
#' @param title_size Title font size as a relative value.
#' @param axis_title_size Axis title font size as a relative value.
#' @param legend_title_size Legend title font size as a relative value.
#' @param legend_text_size Legend text font size as a relative value.
#' @param axis_text_size Axis text font size as a relative value.
#' @param ... Additional theme elements to be passed to `ggplot2::theme()`.
#'
#' @return A ggplot2 theme.
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # Using Arial font
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + ama_theme()
#' # Using Times New Roman font
#' ggplot(mtcars, aes(mpg, wt)) + geom_point() + ama_theme(base_family = "serif")
#' }
ama_theme <- function(base_size = 16,
                      base_family = "sans", 
                      title_size = ggplot2::rel(1.2),
                      axis_title_size = ggplot2::rel(1.2),
                      legend_title_size = ggplot2::rel(0.6),
                      legend_text_size = ggplot2::rel(0.6),
                      axis_text_size = ggplot2::rel(1),
                      ...) {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) + 
    ggplot2::theme(
      # remove major gridlines
      panel.grid.major = ggplot2::element_blank(),
      # remove minor gridlines
      panel.grid.minor = ggplot2::element_blank(),
      # remove panel border
      panel.border = ggplot2::element_blank(),
      line = ggplot2::element_line(),
      # change font
      text = ggplot2::element_text(),
      # if you want to remove legend title
      # legend.title = ggplot2::element_blank(),
      # Sentence-style capitalization
      legend.title = ggplot2::element_text(size = legend_title_size, face = "bold"), 
      # change font size of legend
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.background = ggplot2::element_rect(color = "black"),
      # change font size of main title
      plot.title = ggplot2::element_text(
        size = title_size,
        face = "bold",
        hjust = 0.5,
        margin = ggplot2::margin(b = 15)
      ),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"),
      # left align note 
      plot.caption = ggplot2::element_text(hjust = 0),
      # add black line along axes
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.8),
      axis.ticks = ggplot2::element_line(),
      # axis title with Headline-Style Capitalization
      axis.title.x = ggplot2::element_text(size = axis_title_size, face = "bold"), 
      axis.title.y = ggplot2::element_text(size = axis_title_size, face = "bold"), 
      # axis text size
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      axis.text.x = ggplot2::element_text(size = axis_text_size),
      ...  # Passing the additional arguments to theme()
    )
}
