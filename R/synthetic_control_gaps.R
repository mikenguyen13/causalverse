#' Synthetic Control Inference Plot (Abadie-Diamond-Hainmueller Style)
#'
#' Creates the canonical two-panel synthetic control figure: (1) the level
#' plot showing the treated unit against its synthetic control, and (2) the
#' gap plot showing the difference between them, overlaid with donor/placebo
#' gaps in grey. A permutation-based p-value is computed as the fraction of
#' placebos with a post/pre RMSPE ratio at least as large as the treated unit.
#'
#' @title Synthetic Control Gaps and Inference Plot
#'
#' @description Reproduces the visual inference approach from Abadie,
#'   Diamond & Hainmueller (2010). Placebo gaps (donor unit gaps or
#'   permutation draws) are overlaid in grey to provide a null distribution
#'   for assessing statistical significance.
#'
#' @param Y_treated Numeric vector. Observed outcome for the treated unit.
#' @param Y_synthetic Numeric vector. Synthetic control outcome (same length
#'   as `Y_treated`).
#' @param time_points Numeric or integer vector. Time periods (same length as
#'   `Y_treated`).
#' @param treat_time Numeric. The first treatment period.
#' @param donor_gaps A numeric matrix or data frame where each column is the
#'   gap (actual minus synthetic) for one placebo/donor unit. `NULL` to omit
#'   placebo overlay and p-value.
#' @param title Character or `NULL`. Title prefix for both panels.
#' @param alpha Numeric. Significance level for RMSPE-ratio p-value. Default
#'   `0.05`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`plot`}{A patchwork or list of ggplot2 objects (level + gap
#'       panels).}
#'     \item{`p_value`}{Permutation p-value (fraction of placebos with
#'       post/pre RMSPE ratio >= treated unit ratio). `NA` if no donors.}
#'     \item{`rmspe_ratio`}{Post/pre RMSPE ratio for the treated unit.}
#'     \item{`level_plot`}{ggplot2 level panel.}
#'     \item{`gap_plot`}{ggplot2 gap panel.}
#'   }
#'
#' @references
#' Abadie, A., Diamond, A., & Hainmueller, J. (2010). Synthetic control
#' methods for comparative case studies: Estimating the effect of California's
#' tobacco control program. *Journal of the American Statistical Association*,
#' 105(490), 493-505.
#'
#' @examples
#' # Simulate treated and synthetic outcomes
#' set.seed(7)
#' time_pts <- 1990:2005
#' treat_t  <- 1997
#' Y_tr     <- c(seq(40, 55, length.out = 7),
#'               seq(55, 70, length.out = 9)) + rnorm(16, 0, 1.5)
#' Y_sc     <- c(seq(40, 55, length.out = 7),
#'               seq(55, 59, length.out = 9)) + rnorm(16, 0, 1.5)
#'
#' # Simulate 10 donor gaps
#' donor_mat <- matrix(rnorm(16 * 10, 0, 3), nrow = 16)
#'
#' result <- sc_inference_plot(
#'   Y_treated    = Y_tr,
#'   Y_synthetic  = Y_sc,
#'   time_points  = time_pts,
#'   treat_time   = treat_t,
#'   donor_gaps   = donor_mat
#' )
#' result$plot
#' result$p_value
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_vline geom_hline
#'   geom_ribbon scale_color_manual scale_linetype_manual labs theme
#'   element_text
#' @export
sc_inference_plot <- function(Y_treated,
                               Y_synthetic,
                               time_points,
                               treat_time,
                               donor_gaps = NULL,
                               title      = NULL,
                               alpha      = 0.05) {

  n <- length(Y_treated)
  if (length(Y_synthetic) != n) stop("Y_treated and Y_synthetic must have the same length.")
  if (length(time_points)  != n) stop("time_points must have the same length as Y_treated.")

  gap_treated <- Y_treated - Y_synthetic

  # ---- RMSPE ratio for treated unit ----------------------------------------
  pre_idx  <- time_points < treat_time
  post_idx <- time_points >= treat_time

  rmspe_pre  <- sqrt(mean(gap_treated[pre_idx]^2,  na.rm = TRUE))
  rmspe_post <- sqrt(mean(gap_treated[post_idx]^2, na.rm = TRUE))
  rmspe_ratio <- if (rmspe_pre > 0) rmspe_post / rmspe_pre else NA_real_

  # ---- Permutation p-value -------------------------------------------------
  p_value <- NA_real_
  if (!is.null(donor_gaps)) {
    donor_gaps <- as.matrix(donor_gaps)
    if (nrow(donor_gaps) != n) {
      warning("donor_gaps has ", nrow(donor_gaps), " rows but time_points has ", n,
              " elements. Skipping p-value computation.")
      donor_gaps <- NULL
    }
  }
  if (!is.null(donor_gaps)) {
    donor_ratios <- apply(donor_gaps, 2, function(g) {
      r_pre  <- sqrt(mean(g[pre_idx]^2,  na.rm = TRUE))
      r_post <- sqrt(mean(g[post_idx]^2, na.rm = TRUE))
      if (!is.na(r_pre) && r_pre > 0) r_post / r_pre else NA_real_
    })
    donor_ratios <- donor_ratios[!is.na(donor_ratios)]
    if (!is.na(rmspe_ratio) && length(donor_ratios) > 0) {
      p_value <- mean(donor_ratios >= rmspe_ratio)
    }
  }

  t_prefix <- if (!is.null(title)) paste0(title, ": ") else ""

  # ---- Level plot ----------------------------------------------------------
  level_df <- data.frame(
    t        = rep(time_points, 2),
    y        = c(Y_treated, Y_synthetic),
    series   = rep(c("Treated", "Synthetic Control"), each = n),
    stringsAsFactors = FALSE
  )
  level_df$series <- factor(level_df$series,
                             levels = c("Treated", "Synthetic Control"))

  p_level <- ggplot2::ggplot(level_df,
               ggplot2::aes(x = t, y = y, color = series, linetype = series)) +
    ggplot2::geom_vline(xintercept = treat_time, linetype = "dashed",
                        color = "grey30", linewidth = 0.7) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(
      values = c("Treated" = "#d73027", "Synthetic Control" = "#4575b4"),
      name   = NULL) +
    ggplot2::scale_linetype_manual(
      values = c("Treated" = "solid", "Synthetic Control" = "longdash"),
      name   = NULL) +
    ggplot2::labs(
      title = paste0(t_prefix, "Treated vs. Synthetic Control"),
      x     = "Time",
      y     = "Outcome"
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "bottom")

  # ---- Gap plot ------------------------------------------------------------
  gap_df <- data.frame(
    t   = time_points,
    gap = gap_treated,
    stringsAsFactors = FALSE
  )

  p_gap <- ggplot2::ggplot(gap_df, ggplot2::aes(x = t, y = gap))

  # Placebo gaps in grey (drawn first, underneath)
  if (!is.null(donor_gaps)) {
    n_donors <- ncol(donor_gaps)
    for (j in seq_len(min(n_donors, 50))) {  # cap at 50 donors for legibility
      d_df <- data.frame(t = time_points, gap_d = donor_gaps[, j])
      p_gap <- p_gap +
        ggplot2::geom_line(data = d_df,
          ggplot2::aes(x = t, y = gap_d),
          color = "grey75", linewidth = 0.4, alpha = 0.6, inherit.aes = FALSE)
    }
  }

  p_value_label <- if (!is.na(p_value)) {
    sprintf("Permutation p = %.3f", p_value)
  } else {
    ""
  }

  p_gap <- p_gap +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    ggplot2::geom_vline(xintercept = treat_time, linetype = "dashed",
                        color = "grey30", linewidth = 0.7) +
    ggplot2::geom_line(color = "#d73027", linewidth = 1.3) +
    ggplot2::labs(
      title    = paste0(t_prefix, "Gap: Treated - Synthetic Control"),
      subtitle = p_value_label,
      x        = "Time",
      y        = "Gap"
    ) +
    causalverse::ama_theme()

  # ---- Combine -------------------------------------------------------------
  combined <- if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p_level, p_gap, ncol = 1)
  } else {
    message("Install 'patchwork' for a combined two-panel figure.")
    list(level_plot = p_level, gap_plot = p_gap)
  }

  list(
    plot        = combined,
    p_value     = p_value,
    rmspe_ratio = rmspe_ratio,
    level_plot  = p_level,
    gap_plot    = p_gap
  )
}
