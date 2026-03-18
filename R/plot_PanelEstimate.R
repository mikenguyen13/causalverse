#' Plot Estimated Effects of Treatment Over Time
#'
#' This function takes an object (result of PanelEstimate or similar) and plots its estimates over time.
#'
#' @param pe.object The object with the estimation results.
#' @param ylab The y-axis label.
#' @param xlab The x-axis label.
#' @param main The main title for the plot.
#' @param ylim The limits for the y-axis.
#' @param theme_use The theme to use for the plot. Defaults to causalverse::ama_theme().
#' @param ... Additional parameters to pass to labs() function.
#' @return A ggplot object with the desired plot.
#' @export
#' @examples
#' \dontrun{
#' library(PanelMatch)
#' pd <- PanelData(panel.data = dem, unit.id = "wbcode2",
#'                 time.id = "year", treatment = "dem", outcome = "y")
#' PM.results.ps.weight <- PanelMatch(
#'   panel.data = pd, lag = 4, refinement.method = "ps.weight",
#'   match.missing = FALSE, listwise.delete = TRUE,
#'   covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
#'   size.match = 5, qoi = "att", lead = 0:4,
#'   forbid.treatment.reversal = FALSE
#' )
#'
#' PE.results <- PanelEstimate(sets = PM.results.ps.weight,
#'                             panel.data = pd,
#'                             se.method = "bootstrap",
#'                             number.iterations = 1000,
#'                             confidence.level = .95)
#'
#' plot_panel_estimate(PE.results)
#' }
plot_panel_estimate <- function(pe.object,
                               ylab = "Estimated Effect of Treatment",
                               xlab = "Time Since Treatment",
                               main = "Estimated Effects of Treatment Over Time",
                               ylim = NULL,
                               theme_use = causalverse::ama_theme(),
                               ...)
{

  plot.data <- as.data.frame(summary(pe.object, verbose = FALSE))
  plot.data$Time <- base::rownames(plot.data)

  # Get CI column names dynamically (they vary by confidence level)
  ci_cols <- colnames(plot.data)
  ci_lower <- ci_cols[3]
  ci_upper <- ci_cols[4]

  if (is.null(ylim)) {
    ylim <- c(base::min(plot.data[[ci_lower]]) - base::abs(base::mean(plot.data[["estimate"]])),
              base::max(plot.data[[ci_upper]]) + base::abs(base::mean(plot.data[["estimate"]])))
  }

  plot.data$ci_lower <- plot.data[[ci_lower]]
  plot.data$ci_upper <- plot.data[[ci_upper]]

  p <- ggplot2::ggplot(data = plot.data, ggplot2::aes(x = .data$Time, y = .data$estimate)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$estimate), shape = 16, size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$ci_lower,
                                        ymax = .data$ci_upper),
                           width = 0.25) +
    ggplot2::labs(x = xlab, y = ylab, title = main, ...) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = "dashed") +
    theme_use

  return(p)
}

#' @rdname plot_panel_estimate
#' @export
plot_PanelEstimate <- function(...) {
  .Deprecated("plot_panel_estimate")
  plot_panel_estimate(...)
}
