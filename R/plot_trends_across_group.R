#' Custom Faceted Line Plot with Optional Standard Error
#'
#' This function generates a faceted line plot for a given dataset, allowing the user
#' to specify the x-axis, y-axis, grouping variable, and facet variable.
#' Additionally, users can include standard errors and customize labels.
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_var A character string specifying the x-axis variable.
#' @param y_var A character string specifying the y-axis variable.
#' @param grouping_var A character string specifying the grouping variable.
#' @param facet_var A character string specifying the facet variable.
#' @param se A character string specifying the standard error variable, or NULL (default) if not provided.
#' @param include_legend Logical. If TRUE, includes the legend, otherwise it does not.
#' @param title Character string specifying the main plot title.
#' @param x_label Character string specifying the x-axis label.
#' @param y_label Character string specifying the y-axis label.
#' @param theme A ggplot2 theme. Defaults to \code{\link[causalverse]{ama_theme}}.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{labs}}.
#'
#' @examples
#' \dontrun{
#' # Create a small sample dataset
#' sample_data <- data.frame(
#'   year = rep(2001:2005, each = 2),
#'   dependent_variable = rnorm(10, mean = 50, sd = 10),
#'   group = rep(c("treated", "control"), times = 5),
#'   industry = rep(c("Tech", "Healthcare"), each = 5)
#' )
#' 
#' # Use the function
#' plot_trends_across_group(data = sample_data,
#'                         x_var = "year",
#'                         y_var = "dependent_variable",
#'                         grouping_var = "group",
#'                         facet_var = "industry",
#'                         title = "Sample Title")
#' }
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 ggplot aes_string geom_line facet_wrap theme_minimal labs

plot_trends_across_group <- function(data,
                                    x_var,
                                    y_var,
                                    grouping_var,
                                    facet_var,
                                    se = NULL,
                                    include_legend = TRUE,
                                    title = "Dependent Variable across Years by Group and Industry",
                                    x_label = "Year",
                                    y_label = "Dependent Variable",
                                    theme = causalverse::ama_theme(), ...){
  
  # Check if the variables exist in the dataset
  necessary_vars <- c(x_var, y_var, grouping_var, facet_var)
  if (!all(necessary_vars %in% names(data))) {
    stop("One or more of the specified variables do not exist in the dataset.")
  }
  
  # Check if y_var is numeric
  if (!is.numeric(data[[y_var]])) {
    stop(paste0("'", y_var, "' must be numeric."))
  }
  
  # If se is provided, check it as well
  if (!is.null(se)) {
    if (!se %in% names(data)) {
      stop(paste0("'", se, "' does not exist in the dataset."))
    }
    if (!is.numeric(data[[se]])) {
      stop(paste0("'", se, "' must be numeric."))
    }
  }
  
  # Base plot
  plot <- ggplot(data, aes(x = x_var, y = y_var, color = grouping_var)) +
    geom_line() +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = 2) +
    theme_minimal() +
    labs(title = title, x = x_label, y = y_label, color = grouping_var, fill = grouping_var) +
    theme
  
  # Add geom_ribbon if se is provided
  if (!is.null(se)) {
    plot <- plot + 
      geom_ribbon(aes_string(ymin = paste(y_var, "-", se),
                             ymax = paste(y_var, "+", se),
                             fill = grouping_var), alpha = 0.3)
  }
  
  # Remove legend if not desired
  if (!include_legend) {
    plot <- plot + theme(legend.position = "none")
  }
  
  # Add additional labelling arguments from ...
  if (!missing(...)) {
    plot <- plot + labs(...)
  }
  
  return(plot)
}
