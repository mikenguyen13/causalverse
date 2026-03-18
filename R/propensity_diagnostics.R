#' Comprehensive Propensity Score Diagnostics
#'
#' Produces a set of diagnostic plots and statistics for evaluating a
#' propensity score model: (1) an overlap density plot, (2) a histogram by
#' treatment group with trimming bounds, and (3) a calibration plot comparing
#' predicted treatment probability with observed treatment rate by decile.
#' The three panels are assembled into a combined figure via \pkg{patchwork}
#' if available.
#'
#' @title Propensity Score Diagnostics Panel
#'
#' @description Evaluates overlap, common support, and calibration of
#'   estimated propensity scores. All three plots are returned individually
#'   and as a combined figure.
#'
#' @param ps_scores Numeric vector of estimated propensity scores (values in
#'   (0, 1)).
#' @param treatment Integer or logical vector of the same length as
#'   `ps_scores`. Treatment indicator (1 = treated, 0 = control).
#' @param data Data frame or `NULL`. If supplied, it is used only for
#'   labelling purposes.
#' @param method Character. Label for the propensity score estimation method
#'   shown in plot titles (e.g. `"logit"`, `"rf"`). Default `"any"`.
#' @param title Character or `NULL`. Optional overall title prefix.
#'
#' @return A named list:
#'   \describe{
#'     \item{`overlap_plot`}{ggplot2 density overlap plot.}
#'     \item{`histogram_plot`}{ggplot2 histogram by treatment group.}
#'     \item{`calibration_plot`}{ggplot2 calibration plot.}
#'     \item{`combined_plot`}{Patchwork combined figure (or `overlap_plot`
#'       if \pkg{patchwork} is unavailable).}
#'     \item{`overlap_stats`}{Named numeric vector: `min_treated`,
#'       `max_control`, `common_support_lo`, `common_support_hi`,
#'       `pct_off_support`.}
#'   }
#'
#' @examples
#' set.seed(42)
#' n   <- 400
#' x1  <- rnorm(n)
#' x2  <- rnorm(n)
#' lp  <- -0.5 + 0.8 * x1 + 0.5 * x2
#' ps  <- 1 / (1 + exp(-lp))
#' trt <- rbinom(n, 1, ps)
#'
#' diag <- propensity_diagnostics(ps_scores = ps, treatment = trt)
#' diag$combined_plot
#' diag$overlap_stats
#'
#' @importFrom ggplot2 ggplot aes geom_density geom_histogram geom_point
#'   geom_line geom_abline geom_vline scale_fill_manual scale_color_manual
#'   labs theme facet_wrap
#' @importFrom stats quantile
#' @export
propensity_diagnostics <- function(ps_scores,
                                    treatment,
                                    data   = NULL,
                                    method = c("logit", "rf", "any"),
                                    title  = NULL) {

  method <- match.arg(method)

  if (length(ps_scores) != length(treatment)) {
    stop("ps_scores and treatment must have the same length.")
  }

  ps_scores <- as.numeric(ps_scores)
  treatment <- as.integer(treatment)
  keep      <- !is.na(ps_scores) & !is.na(treatment) &
               ps_scores > 0 & ps_scores < 1
  ps_scores <- ps_scores[keep]
  treatment <- treatment[keep]

  if (length(ps_scores) < 10) stop("Too few valid observations.")

  grp_lbl   <- c("0" = "Control", "1" = "Treated")
  grp_colors <- c("Control" = "#4575b4", "Treated" = "#d73027")

  plot_df <- data.frame(
    ps  = ps_scores,
    grp = factor(grp_lbl[as.character(treatment)],
                 levels = c("Control", "Treated")),
    stringsAsFactors = FALSE
  )

  title_prefix <- if (!is.null(title)) paste0(title, ": ") else ""

  # ---- 1. Overlap / density plot -------------------------------------------
  p_overlap <- ggplot2::ggplot(plot_df, ggplot2::aes(x = ps, fill = grp, color = grp)) +
    ggplot2::geom_density(alpha = 0.35) +
    ggplot2::scale_fill_manual(values  = grp_colors, name = NULL) +
    ggplot2::scale_color_manual(values = grp_colors, name = NULL) +
    ggplot2::labs(
      title = paste0(title_prefix, "Propensity Score Overlap"),
      x     = "Propensity Score",
      y     = "Density"
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "bottom")

  # ---- 2. Histogram with trimming bounds -----------------------------------
  # Trimming: [max(min PS_treated, min PS_control), min(max PS_treated, max PS_control)]
  ps_t  <- ps_scores[treatment == 1]
  ps_c  <- ps_scores[treatment == 0]
  cs_lo <- max(min(ps_t, na.rm = TRUE), min(ps_c, na.rm = TRUE))
  cs_hi <- min(max(ps_t, na.rm = TRUE), max(ps_c, na.rm = TRUE))

  p_hist <- ggplot2::ggplot(plot_df, ggplot2::aes(x = ps, fill = grp)) +
    ggplot2::geom_histogram(
      position = "identity", bins = 30, alpha = 0.55, color = "white") +
    ggplot2::geom_vline(xintercept = cs_lo, linetype = "dashed",
                        color = "grey30", linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = cs_hi, linetype = "dashed",
                        color = "grey30", linewidth = 0.7) +
    ggplot2::scale_fill_manual(values = grp_colors, name = NULL) +
    ggplot2::labs(
      title    = paste0(title_prefix, "PS Histogram with Trimming Bounds"),
      subtitle = sprintf("Common support: [%.3f, %.3f]", cs_lo, cs_hi),
      x        = "Propensity Score",
      y        = "Count"
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "bottom")

  # ---- 3. Calibration plot (by decile) ------------------------------------
  n_dec   <- 10
  dec_cuts <- stats::quantile(ps_scores, probs = seq(0, 1, 1 / n_dec),
                               na.rm = TRUE)
  dec_idx  <- cut(ps_scores, breaks = dec_cuts,
                  include.lowest = TRUE, labels = FALSE)

  cal_list <- lapply(seq_len(n_dec), function(d) {
    idx <- which(dec_idx == d)
    if (length(idx) == 0) return(NULL)
    data.frame(
      pred_mean = mean(ps_scores[idx]),
      obs_rate  = mean(treatment[idx]),
      n_dec_obs = length(idx),
      stringsAsFactors = FALSE
    )
  })
  cal_df <- do.call(rbind, Filter(Negate(is.null), cal_list))

  p_cal <- ggplot2::ggplot(cal_df,
           ggplot2::aes(x = pred_mean, y = obs_rate)) +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(color = "#d73027", size = 3.5, alpha = 0.9) +
    ggplot2::geom_line(color = "#d73027", linewidth = 1.0) +
    ggplot2::labs(
      title    = paste0(title_prefix, "PS Calibration (by Decile)"),
      subtitle = "Dashed line = perfect calibration",
      x        = "Mean Predicted PS",
      y        = "Observed Treatment Rate"
    ) +
    causalverse::ama_theme()

  # ---- Overlap statistics --------------------------------------------------
  pct_off <- mean(ps_scores < cs_lo | ps_scores > cs_hi) * 100
  overlap_stats <- c(
    min_treated       = min(ps_t, na.rm = TRUE),
    max_treated       = max(ps_t, na.rm = TRUE),
    min_control       = min(ps_c, na.rm = TRUE),
    max_control       = max(ps_c, na.rm = TRUE),
    common_support_lo = cs_lo,
    common_support_hi = cs_hi,
    pct_off_support   = pct_off
  )

  # ---- Combined plot -------------------------------------------------------
  combined_plot <- if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p_overlap, p_hist, p_cal, ncol = 1)
  } else {
    message("Install 'patchwork' for a combined diagnostic panel.")
    p_overlap
  }

  list(
    overlap_plot     = p_overlap,
    histogram_plot   = p_hist,
    calibration_plot = p_cal,
    combined_plot    = combined_plot,
    overlap_stats    = overlap_stats
  )
}
