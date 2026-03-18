#' Plot Bacon Decomposition of TWFE Estimates
#'
#' Creates a scatter plot of the Goodman-Bacon (2021) decomposition, showing
#' how each 2x2 DID comparison contributes to the overall TWFE estimate.
#' Points are sized by weight and colored by comparison type.
#'
#' @param decomp A data frame from \code{bacondecomp::bacon()}, with columns
#'   \code{estimate}, \code{weight}, and \code{type}.
#' @param weighted_avg Numeric. The overall TWFE estimate (weighted average).
#'   If \code{NULL} (default), computed from the decomposition.
#' @param title Character string. Plot title.
#'   Default is \code{"Goodman-Bacon Decomposition"}.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' library(bacondecomp)
#' df <- bacon(y ~ treatment, data = your_data, id_var = "id", time_var = "time")
#' bacon_decomp_plot(df)
#' }
#'
#' @references
#' Goodman-Bacon, A. (2021). "Difference-in-Differences with Variation in
#' Treatment Timing." *Journal of Econometrics*, 225(2), 254-277.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs theme_minimal
#'   scale_size_continuous scale_color_brewer
#' @export
bacon_decomp_plot <- function(decomp,
                              weighted_avg = NULL,
                              title = "Goodman-Bacon Decomposition") {

  if (!all(c("estimate", "weight", "type") %in% names(decomp))) {
    stop("`decomp` must have columns: estimate, weight, type")
  }

  if (is.null(weighted_avg)) {
    weighted_avg <- sum(decomp$estimate * decomp$weight)
  }

  ggplot2::ggplot(decomp, ggplot2::aes(x = weight, y = estimate)) +
    ggplot2::geom_hline(
      yintercept = weighted_avg,
      linetype = "dashed", color = "red"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = type, size = weight),
      alpha = 0.7
    ) +
    ggplot2::scale_size_continuous(range = c(2, 8), guide = "none") +
    ggplot2::labs(
      x = "Weight",
      y = "2x2 DID Estimate",
      title = title,
      color = "Comparison Type",
      caption = paste("Weighted average (TWFE):", round(weighted_avg, 4))
    ) +
    ggplot2::theme_minimal()
}
