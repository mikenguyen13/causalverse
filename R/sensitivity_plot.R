#' Plot Treatment Effect Sensitivity to Unobserved Confounding
#'
#' Creates a plot showing how treatment effect estimates change under
#' different assumptions about the degree of unobserved confounding,
#' following the framework of Cinelli and Hazlett (2020).
#'
#' @param estimate Numeric. The point estimate of the treatment effect.
#' @param se Numeric. The standard error of the estimate.
#' @param r2dz_x Numeric vector. Partial R-squared of the confounder with
#'   the treatment (x-axis values to plot). Default is \code{seq(0, 0.3, 0.01)}.
#' @param r2yz_dx Numeric vector. Partial R-squared of the confounder with
#'   the outcome. Default is \code{seq(0, 0.3, 0.01)}.
#' @param benchmark_r2 Optional named list of benchmark partial R-squared
#'   values, e.g., \code{list("Age" = c(r2dz = 0.05, r2yz = 0.1))}.
#' @param conf_level Numeric. Confidence level. Default is 0.95.
#' @param title Character. Plot title.
#'
#' @return A ggplot2 object showing contour lines of adjusted estimates.
#'
#' @references
#' Cinelli, C. and Hazlett, C. (2020). "Making Sense of Sensitivity:
#' Extending Omitted Variable Bias." *Journal of the Royal Statistical
#' Society: Series B*, 82(1), 39-67.
#'
#' @importFrom ggplot2 ggplot aes geom_contour geom_point labs theme_minimal
#' @export
sensitivity_plot <- function(estimate,
                             se,
                             r2dz_x = seq(0, 0.3, 0.01),
                             r2yz_dx = seq(0, 0.3, 0.01),
                             benchmark_r2 = NULL,
                             conf_level = 0.95,
                             title = "Sensitivity to Unobserved Confounding") {

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  # Create grid of bias-adjusted estimates
  grid <- expand.grid(r2dz = r2dz_x, r2yz = r2yz_dx)

  # Approximate bias adjustment (simplified OVB formula)
  # bias ~ sqrt(r2yz * r2dz / (1 - r2dz)) * sign(estimate)
  grid$bias <- sqrt(grid$r2yz * grid$r2dz / pmax(1 - grid$r2dz, 0.001))
  grid$adjusted <- estimate - grid$bias * sign(estimate)
  grid$significant <- abs(grid$adjusted) > z * se

  p <- ggplot2::ggplot(grid, ggplot2::aes(x = r2dz, y = r2yz)) +
    ggplot2::geom_raster(ggplot2::aes(fill = adjusted), alpha = 0.5) +
    ggplot2::geom_contour(
      ggplot2::aes(z = adjusted),
      color = "gray30",
      breaks = c(0, estimate / 2, estimate)
    ) +
    ggplot2::scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0, name = "Adjusted\nEstimate"
    ) +
    ggplot2::labs(
      x = expression(R^2 ~ "of confounder with treatment"),
      y = expression(R^2 ~ "of confounder with outcome"),
      title = title
    ) +
    ggplot2::theme_minimal()

  # Add benchmarks
  if (!is.null(benchmark_r2)) {
    bm_df <- do.call(rbind, lapply(names(benchmark_r2), function(name) {
      vals <- benchmark_r2[[name]]
      data.frame(
        r2dz = vals["r2dz"], r2yz = vals["r2yz"],
        label = name, stringsAsFactors = FALSE
      )
    }))
    p <- p +
      ggplot2::geom_point(
        data = bm_df,
        ggplot2::aes(x = r2dz, y = r2yz),
        shape = 4, size = 3, stroke = 1.5, color = "black"
      ) +
      ggplot2::geom_text(
        data = bm_df,
        ggplot2::aes(x = r2dz, y = r2yz, label = label),
        hjust = -0.2, vjust = -0.5, size = 3
      )
  }

  p
}
