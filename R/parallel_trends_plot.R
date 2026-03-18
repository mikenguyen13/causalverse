#' Parallel Trends Visualization for Difference-in-Differences
#'
#' Plots mean outcome trajectories for treated and control groups over time,
#' with shaded pre- and post-treatment regions, 95% confidence interval
#' ribbons, a vertical dashed line at the treatment period, and a visual
#' annotation highlighting the pre-treatment parallel trends assumption.
#'
#' @title Parallel Trends Plot for DiD
#'
#' @description Creates a publication-ready time-series plot comparing treated
#'   and control group mean outcomes before and after treatment. Pre-treatment
#'   parallelism is annotated visually to help assess the key identifying
#'   assumption of difference-in-differences designs.
#'
#' @param data A data frame in long (panel) format.
#' @param unit_var Character. Name of the unit/panel identifier column.
#' @param time_var Character. Name of the time period column (numeric or integer).
#' @param outcome_var Character. Name of the outcome variable column (numeric).
#' @param treatment_var Character. Name of the binary treatment indicator
#'   column (0/1 or logical). Treatment status is taken as the **ever-treated**
#'   indicator (max value per unit).
#' @param treat_time Numeric. The first treatment period (vertical line
#'   placement and shading boundary).
#' @param title Character or `NULL`. Plot title. Default `NULL` uses a
#'   generic title.
#'
#' @return A `ggplot2` object.
#'
#' @references
#' Angrist, J. D., & Pischke, J.-S. (2009). *Mostly Harmless Econometrics*.
#' Princeton University Press.
#'
#' @examples
#' if (requireNamespace("fixest", quietly = TRUE)) {
#'   data("base_stagg", package = "fixest")
#'   parallel_trends_plot(
#'     data          = base_stagg,
#'     unit_var      = "id",
#'     time_var      = "year",
#'     outcome_var   = "y",
#'     treatment_var = "treated",
#'     treat_time    = 0,
#'     title         = "Parallel Trends: base_stagg Data"
#'   )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_vline
#'   geom_rect annotate scale_color_manual scale_fill_manual labs
#'   theme element_text
#' @importFrom dplyr group_by summarise mutate left_join n
#' @importFrom stats qt sd
#' @export
parallel_trends_plot <- function(data,
                                  unit_var,
                                  time_var,
                                  outcome_var,
                                  treatment_var,
                                  treat_time,
                                  title = NULL) {

  # ---- Input checks --------------------------------------------------------
  stopifnot(is.data.frame(data))
  for (v in c(unit_var, time_var, outcome_var, treatment_var)) {
    if (!v %in% names(data)) {
      stop(sprintf("Column '%s' not found in data.", v))
    }
  }

  # ---- Build ever-treated indicator per unit -------------------------------
  unit_treat <- tapply(data[[treatment_var]], data[[unit_var]], max, na.rm = TRUE)
  data[["..ever_treated.."]] <- unit_treat[as.character(data[[unit_var]])]
  data[["..ever_treated.."]] <- ifelse(data[["..ever_treated.."]] >= 1, "Treated", "Control")

  # ---- Compute group-time means and 95% CI ---------------------------------
  t_vals    <- sort(unique(data[[time_var]]))
  groups    <- c("Treated", "Control")

  agg_list <- lapply(groups, function(g) {
    sub <- data[data[["..ever_treated.."]] == g, ]
    do.call(rbind, lapply(t_vals, function(t) {
      y_vec <- sub[[outcome_var]][sub[[time_var]] == t]
      y_vec <- y_vec[!is.na(y_vec)]
      n_obs <- length(y_vec)
      mn    <- if (n_obs > 0) mean(y_vec) else NA_real_
      se    <- if (n_obs > 1) stats::sd(y_vec) / sqrt(n_obs) else NA_real_
      tq    <- if (n_obs > 1) stats::qt(0.975, df = n_obs - 1) else 1.96
      data.frame(
        time_pt   = t,
        grp       = g,
        mean_y    = mn,
        ci_lo_pt  = mn - tq * se,
        ci_hi_pt  = mn + tq * se,
        stringsAsFactors = FALSE
      )
    }))
  })
  plot_df <- do.call(rbind, agg_list)
  plot_df <- plot_df[!is.na(plot_df$mean_y), ]

  # ---- Pre/post shading limits ----------------------------------------------
  t_min <- min(plot_df$time_pt, na.rm = TRUE)
  t_max <- max(plot_df$time_pt, na.rm = TRUE)

  pre_x_mid <- (t_min + treat_time) / 2
  y_range   <- range(c(plot_df$ci_lo_pt, plot_df$ci_hi_pt), na.rm = TRUE)
  annot_y   <- y_range[1] + 0.85 * diff(y_range)

  # ---- Colors --------------------------------------------------------------
  grp_colors <- c("Treated" = "#d73027", "Control" = "#4575b4")
  grp_fills  <- c("Treated" = "#d73027", "Control" = "#4575b4")

  plot_title <- if (!is.null(title)) title else "Parallel Trends: Treated vs. Control"

  # ---- Build plot ----------------------------------------------------------
  p <- ggplot2::ggplot(plot_df,
         ggplot2::aes(x = time_pt, y = mean_y,
                      color = grp, fill = grp)) +
    # Pre-treatment shading
    ggplot2::annotate("rect",
      xmin = t_min - 0.5, xmax = treat_time,
      ymin = -Inf, ymax = Inf,
      fill = "#f0f0f0", alpha = 0.6) +
    # Post-treatment shading
    ggplot2::annotate("rect",
      xmin = treat_time, xmax = t_max + 0.5,
      ymin = -Inf, ymax = Inf,
      fill = "#fee8c8", alpha = 0.4) +
    # CI ribbons
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_lo_pt, ymax = ci_hi_pt),
      alpha = 0.18, color = NA) +
    # Trend lines
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.5) +
    # Treatment line
    ggplot2::geom_vline(xintercept = treat_time,
                        linetype = "dashed", color = "grey30", linewidth = 0.8) +
    # Parallel trends annotation
    ggplot2::annotate("text",
      x     = pre_x_mid,
      y     = annot_y,
      label = "Parallel\nTrends\nPeriod",
      size  = 3, color = "grey40", fontface = "italic", hjust = 0.5) +
    # Treatment time label
    ggplot2::annotate("text",
      x = treat_time + 0.1, y = annot_y,
      label = "Treatment", size = 3, color = "grey30",
      hjust = 0, fontface = "italic") +
    ggplot2::scale_color_manual(values = grp_colors, name = "Group") +
    ggplot2::scale_fill_manual(values  = grp_fills,  name = "Group") +
    ggplot2::labs(
      title = plot_title,
      x     = time_var,
      y     = outcome_var
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "bottom")

  p
}
