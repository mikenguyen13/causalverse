#' Plot RD Always-assigned Share
#'
#' This function creates a plot for the share of always-assigned units 
#' in a Regression Discontinuity (RD) design, either Sharp RD (SRD) or 
#' Fuzzy RD (FRD). It provides options to include various confidence 
#' intervals and reference lines.
#'
#' @param data The output object from the \code{rdbounds} function.
#' @param rd_type The type of RD design, either "SRD" for Sharp RD or "FRD" 
#'        for Fuzzy RD. Default is "SRD".
#' @param x_label The label for the x-axis. Default is "Share of Always-assigned Units".
#' @param y_label The label for the y-axis. Default is "ATE".
#' @param plot_title The title of the plot. Default is an empty string.
#' @param theme_use A ggplot2 theme function to apply to the plot. Default is 
#'        \code{causalverse::ama_theme()}.
#' @param tau Logical, whether to include a vertical line at the estimated 
#'        treatment effect. Default is TRUE.
#' @param tau_CI Logical, whether to include confidence intervals for the 
#'        treatment effect estimate. Default is FALSE.
#' @param bounds_CI Logical, whether to include confidence intervals for the 
#'        manipulation bounds. Default is TRUE.
#' @param ref_line The y-intercept for a reference line. Default is 0.
#' @param ... Additional arguments passed to \code{labs} in ggplot2.
#'
#' @return A ggplot object.
#' @import ggplot2
#' @import formattable
#' @import data.table
#' @import rdbounds
#' @examples
#' \dontrun{
#' set.seed(1)
#' data <- rdbounds::rdbounds_sampledata(10000, covs = FALSE)
#' rdbounds_est_tau <- rdbounds::rdbounds(
#'     y = data$y,
#'     x = data$x,
#'     treatment = data$treatment,
#'     c = 0,
#'     discrete_x = FALSE,
#'     discrete_y = FALSE,
#'     bwsx = c(.2, .5),
#'     bwy = 1,
#'     kernel = "epanechnikov",
#'     orders = 1,
#'     evaluation_ys = seq(from = 0, to = 15, by = 1),
#'     refinement_A = TRUE,
#'     refinement_B = TRUE,
#'     right_effects = TRUE,
#'     potential_taus = c(.025, .05, .1, .2),
#'     yextremes = c(0, 15),
#'     num_bootstraps = 5
#' )
#' plot_rd_aa_share(rdbounds_est_tau)
#' }
#' @export
plot_rd_aa_share <- function(data, 
                             rd_type = "SRD",
                             x_label = "Share of Always-assigned Units", 
                             y_label = "ATE",
                             plot_title = "",
                             theme_use = causalverse::ama_theme(),
                             tau = TRUE,
                             tau_CI = FALSE,
                             bounds_CI = TRUE,
                             ref_line = 0,
                             ...) {
  
  # Determine the correct prefix based on rd_type
  prefix <- if (rd_type == "FRD") "FRD" else "SRD"
  
  # Extract the necessary data from the rdbounds_est_tau object
  df <- as.data.frame(data$estimates[,1][[paste0("TE_", prefix, "_CIs_manipulation")]])
  naive_estimate <- data$estimates[,1][[paste0("TE_", prefix, "_naive")]]
  tau_hat <- data$estimates[,1]$tau_hat
  tau_hat_CI <- data$estimates[,1]$tau_hat_CI
  
  # Create the plot
  p <- ggplot(df, aes(x = potential_taus)) +
    geom_point(aes(y = TE_lower), size = 3, color = "black") +
    geom_point(aes(y = TE_upper), size = 3, color = "black") +
    geom_line(aes(y = TE_lower), linetype = "solid", color = "black") +
    geom_line(aes(y = TE_upper), linetype = "solid", color = "black") +
    geom_line(aes(y = get(paste0("TE_", prefix, "_CIs_manipulation_lower"))), linetype = "dotted", color = "black") +
    geom_line(aes(y = get(paste0("TE_", prefix, "_CIs_manipulation_upper"))), linetype = "dotted", color = "black") +
    annotate("point", x = 0, y = naive_estimate, size = 3, color = "black") +
    labs(x = x_label, y = y_label, title = plot_title, ...) +
    theme_use
  
  # Add reference line
  if (!is.null(ref_line)) {
    p <- p + geom_hline(yintercept = ref_line, linetype = "dashed", color = "red")
  }
  
  # Add vertical line for tau_hat
  if (tau) {
    p <- p + geom_vline(xintercept = round(tau_hat, 3), linetype = "solid", color = "black")
  }
  
  # Add confidence intervals for tau_hat
  if (tau_CI) {
    p <- p + 
      geom_vline(xintercept = round(tau_hat_CI, 3)[1], linetype = "dotted", color = "black") +
      geom_vline(xintercept = round(tau_hat_CI, 3)[2], linetype = "dotted", color = "black")
  }
  
  # Add manipulation bounds confidence intervals
  if (bounds_CI) {
    p <- p + 
      geom_line(aes(y = get(paste0("TE_", prefix, "_CIs_manipulation_lower"))), linetype = "dotted", color = "black") +
      geom_line(aes(y = get(paste0("TE_", prefix, "_CIs_manipulation_upper"))), linetype = "dotted", color = "black")
  }
  
  return(p)
}
