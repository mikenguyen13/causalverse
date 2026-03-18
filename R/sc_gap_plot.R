#' Gap Plot for Synthetic Control Analysis
#'
#' Plots the treatment-synthetic gap (treated outcome minus synthetic control
#' outcome) over time for synthetic control methods, with optional
#' in-space placebo gaps for statistical inference.
#'
#' @param treated_outcome Numeric vector. Observed outcome for the treated unit
#'   over time.
#' @param synthetic_outcome Numeric vector. Synthetic control outcome over time
#'   (same length as \code{treated_outcome}).
#' @param time_periods Numeric vector. Time period labels (same length).
#' @param treatment_period Numeric or integer. The first period of treatment
#'   (vertical line drawn here).
#' @param donor_gaps List or matrix. Optional. Gaps for donor/placebo units
#'   (in-space placebos). Each column or list element should be a numeric
#'   vector of length \code{length(time_periods)}.
#' @param placebo_alpha Numeric. Transparency of placebo gap lines. Default
#'   \code{0.25}.
#' @param placebo_rmspe_filter Numeric. Exclude donor units whose
#'   pre-treatment RMSPE exceeds \code{placebo_rmspe_filter} times the
#'   treated unit's pre-treatment RMSPE. Default \code{2} (common in
#'   Abadie et al.).
#' @param ci_level Numeric. If \code{> 0}, show a band at ±
#'   \code{ci_level}-th percentile of placebo gaps. Default \code{0.9}.
#' @param title Character. Plot title.
#' @param unit_label Character. Label for the treated unit. Default
#'   \code{"Treated"}.
#'
#' @return A ggplot2 object.
#'
#' @references
#' Abadie, A., Diamond, A., & Hainmueller, J. (2010). Synthetic control methods
#' for comparative case studies: Estimating the effect of California's tobacco
#' control program. \emph{Journal of the American Statistical Association},
#' 105(490), 493–505.
#'
#' @examples
#' \dontrun{
#' library(Synth)
#' # After running synth(), extract gaps:
#' sc_gap_plot(
#'   treated_outcome   = Y_treated,
#'   synthetic_outcome = Y_synthetic,
#'   time_periods      = 1970:2000,
#'   treatment_period  = 1989
#' )
#' }
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_hline
#'   geom_vline labs theme_minimal
#' @export
sc_gap_plot <- function(treated_outcome,
                        synthetic_outcome,
                        time_periods,
                        treatment_period,
                        donor_gaps           = NULL,
                        placebo_alpha        = 0.25,
                        placebo_rmspe_filter = 2,
                        ci_level             = 0.9,
                        title                = "Synthetic Control: Gap Plot",
                        unit_label           = "Treated") {

  n <- length(time_periods)
  if (length(treated_outcome) != n || length(synthetic_outcome) != n) {
    stop("All outcome vectors must have length equal to `time_periods`.",
         call. = FALSE)
  }

  gap_main <- treated_outcome - synthetic_outcome

  # --- Pre-treatment RMSPE for treated unit ---
  pre_idx <- which(time_periods < treatment_period)
  rmspe_treated <- sqrt(mean(gap_main[pre_idx]^2))

  df_main <- data.frame(
    time   = time_periods,
    gap    = gap_main,
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot(df_main, ggplot2::aes(x = time, y = gap)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50") +
    ggplot2::geom_vline(xintercept = treatment_period, linetype = "dashed",
                        color = "gray30") +
    ggplot2::labs(
      x     = "Time Period",
      y     = paste0("Gap: ", unit_label, " - Synthetic"),
      title = title,
      caption = sprintf("RMSPE pre-treatment: %.4f", rmspe_treated)
    ) +
    ggplot2::theme_minimal(base_size = 12)

  # --- Add placebo gaps ---
  if (!is.null(donor_gaps)) {
    if (is.matrix(donor_gaps)) {
      donor_gaps <- lapply(seq_len(ncol(donor_gaps)), function(j) donor_gaps[, j])
    }

    # Filter by RMSPE criterion
    valid_donors <- vapply(donor_gaps, function(g) {
      if (length(g) != n) return(FALSE)
      rmspe_d <- sqrt(mean(g[pre_idx]^2))
      rmspe_d <= placebo_rmspe_filter * rmspe_treated
    }, logical(1))

    donor_gaps <- donor_gaps[valid_donors]

    if (length(donor_gaps) > 0) {
      # Build tidy long data for placebo lines
      df_donors <- do.call(rbind, lapply(seq_along(donor_gaps), function(d) {
        data.frame(
          time   = time_periods,
          gap    = donor_gaps[[d]],
          donor  = d,
          stringsAsFactors = FALSE
        )
      }))

      p <- p +
        ggplot2::geom_line(
          data = df_donors,
          ggplot2::aes(x = time, y = gap, group = donor),
          color = "gray60", alpha = placebo_alpha, linewidth = 0.4,
          inherit.aes = FALSE
        )

      # Optional CI band from placebo distribution
      if (ci_level > 0) {
        alpha <- (1 - ci_level) / 2
        band_df <- do.call(rbind, lapply(seq_len(n), function(i) {
          vals <- vapply(donor_gaps, `[`, i, FUN.VALUE = numeric(1))
          data.frame(
            time = time_periods[i],
            lo   = stats::quantile(vals, alpha,   na.rm = TRUE),
            hi   = stats::quantile(vals, 1 - alpha, na.rm = TRUE)
          )
        }))
        p <- p +
          ggplot2::geom_ribbon(
            data = band_df,
            ggplot2::aes(x = time, ymin = lo, ymax = hi),
            fill = "gray70", alpha = 0.3,
            inherit.aes = FALSE
          )
      }
    }
  }

  # Treated unit on top
  p <- p +
    ggplot2::geom_line(color = "#D73027", linewidth = 1.2)

  p
}
