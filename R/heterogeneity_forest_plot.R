#' Publication-Ready Forest Plot for Subgroup / CATE Analysis
#'
#' Creates a publication-ready forest plot visualising treatment effect
#' heterogeneity across subgroups or conditional average treatment effects
#' (CATE). Confidence intervals are shown as horizontal whiskers, the point
#' estimate as a filled dot colour-coded by statistical significance, and an
#' optional overall-effect diamond is drawn at the bottom.
#'
#' @title Heterogeneity Forest Plot
#'
#' @description Accepts a data frame of subgroup estimates and produces a
#'   forest plot suitable for publication. Subgroups can be sorted by
#'   effect size. An optional overall estimate is shown as a diamond.
#'
#' @param estimates A data frame with at minimum these columns:
#'   \describe{
#'     \item{`label`}{Character. Subgroup label.}
#'     \item{`estimate`}{Numeric. Point estimate.}
#'     \item{`ci_lower`}{Numeric. Lower confidence interval bound.}
#'     \item{`ci_upper`}{Numeric. Upper confidence interval bound.}
#'   }
#'   Optional columns: `n` (sample size, shown as text), `p_value`
#'   (used for significance colouring).
#' @param subgroup Character or `NULL`. Column name in `estimates` to use as
#'   a grouping factor for colouring. Default `NULL`.
#' @param overall A single-row data frame with columns `estimate`,
#'   `ci_lower`, `ci_upper` for the overall effect diamond. `NULL` to omit.
#' @param xlab Character. x-axis label. Default `"Estimated Effect"`.
#' @param title Character. Plot title. Default `"Subgroup Analysis"`.
#' @param ref_line Numeric. x-intercept for the reference line. Default `0`.
#' @param sort_by Character. `"estimate"` to sort subgroups by effect size,
#'   `"none"` to preserve input order. Default `"estimate"`.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' est_df <- data.frame(
#'   label    = c("Female", "Male", "Age < 40", "Age >= 40",
#'                "Low income", "High income"),
#'   estimate = c(0.18, 0.09, 0.22, 0.07, 0.25, 0.05),
#'   ci_lower = c(0.08, -0.01, 0.11, -0.04, 0.13, -0.06),
#'   ci_upper = c(0.28, 0.19, 0.33, 0.18, 0.37, 0.16),
#'   n        = c(150, 150, 140, 160, 130, 170),
#'   p_value  = c(0.001, 0.08, 0.001, 0.26, 0.001, 0.40),
#'   stringsAsFactors = FALSE
#' )
#'
#' overall_df <- data.frame(estimate = 0.13, ci_lower = 0.07, ci_upper = 0.19)
#'
#' heterogeneity_forest_plot(
#'   estimates = est_df,
#'   overall   = overall_df,
#'   title     = "Subgroup Treatment Effects",
#'   sort_by   = "estimate"
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_vline
#'   geom_polygon scale_color_manual scale_y_discrete labs theme
#'   element_text annotate
#' @export
heterogeneity_forest_plot <- function(estimates,
                                       subgroup = NULL,
                                       overall  = NULL,
                                       xlab     = "Estimated Effect",
                                       title    = "Subgroup Analysis",
                                       ref_line = 0,
                                       sort_by  = c("estimate", "none")) {

  sort_by <- match.arg(sort_by)

  required_cols <- c("label", "estimate", "ci_lower", "ci_upper")
  missing_cols  <- setdiff(required_cols, names(estimates))
  if (length(missing_cols) > 0) {
    stop("estimates data frame is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  df <- estimates
  df <- df[!is.na(df$estimate), ]

  # ---- Significance colouring ----------------------------------------------
  if ("p_value" %in% names(df)) {
    df[["sig_label"]] <- ifelse(df$p_value < 0.05, "p < 0.05", "p >= 0.05")
  } else {
    df[["sig_label"]] <- "p >= 0.05"
  }

  # ---- Sorting -------------------------------------------------------------
  if (sort_by == "estimate") {
    df <- df[order(df$estimate), ]
  }
  df$label <- factor(df$label, levels = df$label)

  # ---- n label (right side) -----------------------------------------------
  has_n <- "n" %in% names(df)

  # ---- Overall row ---------------------------------------------------------
  has_overall <- !is.null(overall) &&
    all(c("estimate", "ci_lower", "ci_upper") %in% names(overall))

  # ---- Build plot ----------------------------------------------------------
  sig_colors <- c("p < 0.05"  = "#d73027",
                  "p >= 0.05" = "#4575b4")

  p <- ggplot2::ggplot(df,
         ggplot2::aes(y = label, x = estimate, color = sig_label)) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed",
                        color = "grey40", linewidth = 0.7) +
    ggplot2::geom_segment(
      ggplot2::aes(x = ci_lower, xend = ci_upper, yend = label),
      linewidth = 0.9, alpha = 0.8) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_color_manual(
      values = sig_colors,
      name   = "Significance") +
    ggplot2::labs(
      title = title,
      x     = xlab,
      y     = NULL
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.y     = ggplot2::element_text(size = 10)
    )

  # Add sample size labels to the right
  if (has_n) {
    x_max    <- max(df$ci_upper, na.rm = TRUE)
    x_offset <- diff(range(c(df$ci_lower, df$ci_upper), na.rm = TRUE)) * 0.05
    p <- p +
      ggplot2::annotate("text",
        x     = x_max + x_offset,
        y     = as.numeric(df$label),
        label = paste0("n=", df$n),
        hjust = 0, size = 3, color = "grey40")
  }

  # Overall diamond
  if (has_overall) {
    ov  <- overall
    est <- ov$estimate[1]
    lo  <- ov$ci_lower[1]
    hi  <- ov$ci_upper[1]
    y0  <- 0  # below first factor level

    # Diamond polygon coordinates
    diamond_df <- data.frame(
      x = c(lo,   est,  hi,   est),
      y = c(y0,   y0 + 0.35, y0,  y0 - 0.35)
    )

    p <- p +
      ggplot2::annotate("polygon",
        x    = diamond_df$x,
        y    = diamond_df$y,
        fill = "#1a9641", color = "#1a9641", alpha = 0.8) +
      ggplot2::annotate("text",
        x = est, y = y0 - 0.5,
        label = paste0("Overall: ", round(est, 3)),
        size = 3.2, color = "#1a9641", fontface = "bold", hjust = 0.5)
  }

  p
}
