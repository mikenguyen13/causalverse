#' Plot Event Study Coefficients from Multiple Estimators
#'
#' Creates a combined coefficient plot comparing event study estimates from
#' multiple estimators (e.g., TWFE, Sun-Abraham, Callaway-Sant'Anna, Gardner).
#' This is useful for showing robustness of dynamic treatment effects across
#' different estimation strategies.
#'
#' @param estimates A named list of data frames, each with columns
#'   \code{term} (relative time period), \code{estimate}, and \code{std_error}.
#'   The names of the list become the legend labels.
#' @param reference_period Numeric. The reference period (omitted category).
#'   Default is \code{-1}.
#' @param conf_level Numeric. Confidence level for error bars. Default is 0.95.
#' @param title Character string. Plot title. Default is \code{NULL}.
#' @param xlab Character string. X-axis label. Default is \code{"Periods Relative to Treatment"}.
#' @param ylab Character string. Y-axis label. Default is \code{"Estimate"}.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # After running different estimators:
#' estimates <- list(
#'   "TWFE" = data.frame(term = -5:5, estimate = rnorm(11), std_error = 0.1),
#'   "Sun-Abraham" = data.frame(term = -5:5, estimate = rnorm(11), std_error = 0.1)
#' )
#' plot_event_coefs(estimates)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#'   geom_vline position_dodge labs theme_minimal scale_color_brewer
#' @export
plot_event_coefs <- function(estimates,
                             reference_period = -1,
                             conf_level = 0.95,
                             title = NULL,
                             xlab = "Periods Relative to Treatment",
                             ylab = "Estimate") {

  if (!is.list(estimates) || is.null(names(estimates))) {
    stop("`estimates` must be a named list of data frames.")
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  # Combine all estimates
  combined <- do.call(rbind, lapply(names(estimates), function(name) {
    df <- estimates[[name]]
    if (!all(c("term", "estimate", "std_error") %in% names(df))) {
      stop("Each data frame must have columns: term, estimate, std_error")
    }
    df$estimator <- name
    df$ci_lower <- df$estimate - z * df$std_error
    df$ci_upper <- df$estimate + z * df$std_error
    df
  }))

  n_estimators <- length(unique(combined$estimator))
  dodge_width <- 0.3

  p <- ggplot2::ggplot(
    combined,
    ggplot2::aes(
      x = term, y = estimate,
      color = estimator,
      group = estimator
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(
      xintercept = reference_period + 0.5,
      linetype = "dotted", color = "gray50"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2,
      position = ggplot2::position_dodge(width = dodge_width),
      alpha = 0.6
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = dodge_width),
      size = 2
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      color = "Estimator"
    ) +
    ggplot2::theme_minimal()

  if (n_estimators <= 8) {
    p <- p + ggplot2::scale_color_brewer(palette = "Set1")
  }

  p
}
