#' Compare Treatment Effect Estimates Across Models
#'
#' Creates a forest plot comparing point estimates and confidence intervals
#' from multiple models or specifications. Useful for showing robustness of
#' causal estimates across different identification strategies.
#'
#' @param models A named list of model objects (from \code{lm}, \code{fixest::feols},
#'   etc.) or a data frame with columns \code{model}, \code{estimate},
#'   \code{std_error}.
#' @param coef_name Character string. The name of the coefficient to extract
#'   from each model. Required when \code{models} is a list of model objects.
#' @param conf_level Numeric. Confidence level for intervals. Default is 0.95.
#' @param title Character string. Plot title. Default is \code{"Treatment Effect Comparison"}.
#' @param xlab Character string. X-axis label. Default is \code{"Estimate"}.
#'
#' @return A ggplot2 object showing a forest plot.
#'
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ am, data = mtcars)
#' m2 <- lm(mpg ~ am + wt, data = mtcars)
#' m3 <- lm(mpg ~ am + wt + hp, data = mtcars)
#'
#' plot_coef_comparison(
#'   models = list("Bivariate" = m1, "With Weight" = m2, "Full" = m3),
#'   coef_name = "am"
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_vline
#'   labs theme_minimal
#' @export
plot_coef_comparison <- function(models,
                                 coef_name = NULL,
                                 conf_level = 0.95,
                                 title = "Treatment Effect Comparison",
                                 xlab = "Estimate") {

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  if (is.data.frame(models)) {
    if (!all(c("model", "estimate", "std_error") %in% names(models))) {
      stop("Data frame must have columns: model, estimate, std_error")
    }
    df <- models
  } else {
    if (is.null(coef_name)) {
      stop("`coef_name` is required when `models` is a list of model objects.")
    }
    df <- do.call(rbind, lapply(names(models), function(name) {
      fit <- models[[name]]
      coefs <- tryCatch({
        if (inherits(fit, "fixest")) {
          ct <- fixest::coeftable(fit)
          data.frame(
            estimate = ct[coef_name, 1],
            std_error = ct[coef_name, 2]
          )
        } else {
          ct <- summary(fit)$coefficients
          data.frame(
            estimate = ct[coef_name, 1],
            std_error = ct[coef_name, 2]
          )
        }
      }, error = function(e) {
        warning("Could not extract '", coef_name, "' from model '", name, "'")
        data.frame(estimate = NA_real_, std_error = NA_real_)
      })
      coefs$model <- name
      coefs
    }))
  }

  df$ci_lower <- df$estimate - z * df$std_error
  df$ci_upper <- df$estimate + z * df$std_error
  df$model <- factor(df$model, levels = rev(df$model))

  ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = model)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lower, xmax = ci_upper),
      width = 0.2, color = "gray40", orientation = "y"
    ) +
    ggplot2::geom_point(size = 3, color = "black") +
    ggplot2::labs(
      x = xlab,
      y = NULL,
      title = title
    ) +
    ggplot2::theme_minimal()
}
