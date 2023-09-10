#' Plot Covariate Balance Over Pre-Treatment Period
#'
#' This function visualizes the covariate balance over the pre-treatment period. 
#' It's particularly designed for outputs from methods like PanelMatch.
#'
#' @param balance_data A matrix containing the covariate balance data over the pre-treatment period.
#' @param y_limits A numeric vector of length 2 defining the y-axis limits.
#' @param theme_use A ggplot2 theme. By default, it uses `causalverse::ama_theme()`.
#' @param xlab A string indicating the label for the x-axis.
#' @param ylab A string indicating the label for the y-axis.
#' @param main_title A string for the main title of the plot.
#' @param legend_title A string for the legend title.
#' @param show_legend A logical; if TRUE, the legend is displayed, otherwise, it's hidden.
#' @param ... Additional arguments passed to the ggplot labs.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#'   balance_data_sample <- matrix(rnorm(20), nrow = 5)
#'   plot_covariate_balance_pretrend(balance_data_sample)
#' }
#'
#' @export
plot_covariate_balance_pretrend <- function(balance_data,
                                            y_limits = c(-1, 1),
                                            theme_use = causalverse::ama_theme(),
                                            xlab = "Time to Treatment",
                                            ylab = "Balance (in SD unit)",
                                            main_title = "Covariate Balance Over Pre-Treatment Period",
                                            legend_title = "Covariate",
                                            show_legend = TRUE,
                                            ...) {
  
  # Addresses the R CMD CHECK notes
  Time <- Balance <- Covariate <- NULL
  
  
  # Ensure the input is a matrix
  if (!is.matrix(balance_data)) {
    stop("Input should be a matrix")
  }
  
  # Convert the matrix to a dataframe and then to a long format
  df <- as.data.frame(balance_data)
  df$Time <- rownames(df)
  df_long <- tidyr::gather(df, key = "Covariate", value = "Balance",-Time)
  
  # Convert Time labels from t_1, t_2, etc. to -1, -2, etc.
  df_long$Time <- as.numeric(gsub("t_", "-", df_long$Time))
  
  # Sort Time column to ensure the x-axis is in the correct order
  df_long$Time <- factor(df_long$Time, levels = unique(df_long$Time))
  
  # Plot using ggplot2
  p <- ggplot2::ggplot(df_long,
                       aes(
                         x = Time,
                         y = Balance,
                         color = Covariate,
                         group = Covariate
                       )) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(
      aes(xintercept = "-1"),
      linetype = "dotted",
      color = "black",
      linewidth = 1
    ) +  # vertical dotted line at -1
    ggplot2::geom_hline(
      aes(yintercept = 0),
      linetype = "dotted",
      color = "black",
      linewidth = 1
    ) +   # horizontal dotted line at 0
    ggplot2::labs(
      title = main_title,
      x = xlab,
      y = ylab,
      color = legend_title,
      ...
    ) +
    ggplot2::coord_cartesian(ylim = y_limits) +
    theme_use
  
  # Conditionally remove legend if not required
  if (!show_legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }
  
  return(p)
}
