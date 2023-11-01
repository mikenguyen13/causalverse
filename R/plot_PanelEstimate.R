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
#' PM.results.ps.weight <- PanelMatch(lag = 4, time.id = "year", unit.id = "wbcode2",
#'                                    treatment = "dem", refinement.method = "ps.weight",
#'                                    data = dem, match.missing = FALSE, listwise.delete = TRUE,
#'                                    covs.formula = ~ I(lag(tradewb, 1:4)) + I(lag(y, 1:4)),
#'                                    size.match = 5, qoi = "att", outcome.var = "y",
#'                                    lead = 0:4, forbid.treatment.reversal = FALSE)
#'
#' PE.results <- PanelEstimate(sets = PM.results.ps.weight, 
#'                             data = dem,
#'                             se.method = "bootstrap",
#'                             number.iterations = 1000,
#'                             confidence.level = .95)
#'
#' plot_PanelEstimate(PE.results)
#' }
plot_PanelEstimate <- function(pe.object,
                               ylab = "Estimated Effect of Treatment",
                               xlab = "Time Since Treatment",
                               main = "Estimated Effects of Treatment Over Time",
                               ylim = NULL,
                               theme_use = causalverse::ama_theme(),
                               ...)
{
  
  plot.data <- as.data.frame(summary(pe.object, verbose = F))
  plot.data$Time <- base::rownames(plot.data)
  
  if (is.null(ylim)) {
    ylim <- c(base::min(plot.data[, 3]) - base::abs(base::mean(plot.data[, 1])),
              base::max(plot.data[, 4]) + base::abs(base::mean(plot.data[, 1])))
  }
  
  p <- ggplot2::ggplot(data = plot.data, ggplot2::aes(x = .data$Time, y = .data$estimate)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$estimate), shape = 16, size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[, 3],
                                        ymax = .data[, 4]),
                           width = 0.25) +
    ggplot2::labs(x = xlab, y = ylab, title = main, ...) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = "dashed") +
    theme_use
  
  return(p)
}
