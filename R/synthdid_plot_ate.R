#' Create ATE Plot Using ggplot2
#'
#' This function creates a ggplot for visualizing Average Treatment Effect (ATE)
#' from a given estimation object.
#'
#' @param est Estimation object from `synthdid_est_ate`.
#' @param show_CI Logical; if TRUE, shows confidence intervals on the plot.
#' @param custom_title String; title of the plot.
#' @param xlab String; label for the x-axis.
#' @param ylab String; label for the y-axis.
#' @param y_intercept Numeric; value at which a horizontal line is drawn.
#' @param theme ggplot theme; default is set to causalverse::ama_theme().
#' @param fill_color String; color used for the confidence interval shading.
#' @return A ggplot object representing the ATE plot.
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#'   # Load required libraries
#'   library(ggplot2)
#'   library(tidyverse)
#'   library(causalverse)
#'   
#'   library(tidyverse)
#'   data <- fixest::base_stagg |>
#'     dplyr::mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
#'     dplyr::mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar)))
#'   est <- 
#'     synthdid_est_ate(
#'       data = data,
#'       adoption_cohorts = 5:7,
#'       lags = 2,
#'       leads = 2,
#'       time_var = "year",
#'       unit_id_var = "id",
#'       treated_period_var = "year_treated",
#'       treat_stat_var = "treatvar",
#'       pooled = FALSE,
#'       placebo = FALSE,
#'       outcome_var = "y"
#'     )
#'   # Generate the plot
#'   synthdid_plot_ate(est, show_CI = TRUE, custom_title = "Sample ATE Plot")
#' }
synthdid_plot_ate <- function(
    est,
    show_CI = TRUE,
    custom_title = "",
    xlab = "Relative Time Period", 
    ylab = "ATE", 
    y_intercept = 0,
    theme = causalverse::ama_theme(),
    fill_color = "lightgrey"
) {
  # Prepare data frame from the estimation object
  df <- data.frame(
    x = est$time,
    y = est$TE_mean_w[1:length(est$time)],
    lower_ci = est$TE_mean_w_lower[1:length(est$time)],
    upper_ci = est$TE_mean_w_upper[1:length(est$time)]
  )
  
  # Base ggplot with line and point
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = y_intercept, color = "red") +
    ylim(c(min(df$lower_ci), max(df$upper_ci)))
  
  # Add confidence intervals if requested
  if (show_CI) {
    p <- ggplot(df, aes(x = x, y = y)) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = fill_color, color = "transparent") +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = y_intercept, color = "red") +
      ylim(c(min(est$TE_mean_w_lower), max(est$TE_mean_w_upper)))
  }
  
  # Add labels and apply the specified theme
  p <- p +
    labs(
      title = custom_title,
      x = xlab,
      y = ylab
    ) +
    theme
  
  return(p)
}
