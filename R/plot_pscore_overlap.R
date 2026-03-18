#' Propensity Score Overlap Diagnostic Plot
#'
#' Creates a density plot comparing the distribution of propensity scores
#' for treated and control units. Useful for assessing the overlap (common
#' support) assumption required by many causal inference estimators.
#'
#' @param pscore Numeric vector of estimated propensity scores (values
#'   between 0 and 1).
#' @param treatment Binary vector (0/1 or logical) indicating treatment
#'   assignment. Must be the same length as \code{pscore}.
#' @param trim_bounds Numeric vector of length 2 specifying lower and upper
#'   bounds for trimming (e.g., \code{c(0.1, 0.9)}). If provided, vertical
#'   lines are drawn at these thresholds and observations outside the bounds
#'   are shaded. Default is \code{NULL} (no trimming lines).
#' @param title Character string. Plot title. Default is
#'   \code{"Propensity Score Overlap"}.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' # Simulate data
#' set.seed(42)
#' n <- 500
#' x <- rnorm(n)
#' treat <- rbinom(n, 1, plogis(0.5 * x))
#' ps <- plogis(0.5 * x + rnorm(n, 0, 0.1))
#'
#' plot_pscore_overlap(pscore = ps, treatment = treat)
#' plot_pscore_overlap(pscore = ps, treatment = treat,
#'                     trim_bounds = c(0.1, 0.9))
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_density geom_vline annotate
#'   labs theme_minimal scale_fill_manual scale_color_manual
#' @export
plot_pscore_overlap <- function(pscore,
                                treatment,
                                trim_bounds = NULL,
                                title = "Propensity Score Overlap") {

  # --- Input validation ---
  if (length(pscore) != length(treatment)) {
    stop("`pscore` and `treatment` must have the same length.", call. = FALSE)
  }

  if (!all(treatment %in% c(0, 1, TRUE, FALSE))) {
    stop("`treatment` must be a binary vector (0/1 or logical).", call. = FALSE)
  }

  if (!is.null(trim_bounds)) {
    if (length(trim_bounds) != 2 || trim_bounds[1] >= trim_bounds[2]) {
      stop("`trim_bounds` must be a numeric vector of length 2 with lower < upper.",
           call. = FALSE)
    }
  }

  # --- Build plot data ---
  plot_df <- data.frame(
    pscore = pscore,
    group  = factor(
      ifelse(as.integer(treatment) == 1, "Treated", "Control"),
      levels = c("Control", "Treated")
    )
  )

  # --- Density plot ---
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = pscore, fill = group)) +
    ggplot2::geom_density(alpha = 0.4, color = NA) +
    ggplot2::geom_density(
      ggplot2::aes(color = group),
      fill = NA, linewidth = 0.6
    ) +
    ggplot2::scale_fill_manual(
      values = c("Control" = "#3182bd", "Treated" = "#e6550d")
    ) +
    ggplot2::scale_color_manual(
      values = c("Control" = "#3182bd", "Treated" = "#e6550d")
    ) +
    ggplot2::labs(
      x     = "Propensity Score",
      y     = "Density",
      fill  = "Group",
      color = "Group",
      title = title
    ) +
    ggplot2::theme_minimal()

  # --- Optional trimming bounds ---
  if (!is.null(trim_bounds)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = trim_bounds,
        linetype   = "dashed",
        color      = "gray30"
      ) +
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = trim_bounds[1],
        ymin = -Inf, ymax = Inf,
        fill = "gray80", alpha = 0.3
      ) +
      ggplot2::annotate(
        "rect",
        xmin = trim_bounds[2], xmax = Inf,
        ymin = -Inf, ymax = Inf,
        fill = "gray80", alpha = 0.3
      )
  }

  p
}
