#' Enhanced Event Study Plot for Difference-in-Differences
#'
#' Creates a comprehensive event study (dynamic DiD) plot with confidence
#' bands, reference period annotation, pre-trends test annotation, and
#' support for multiple estimators on the same plot. Works with
#' \pkg{fixest}, \pkg{did} (Callaway-Sant'Anna), and manually supplied
#' coefficients.
#'
#' @param models Named list of model objects or data frames. Each element is
#'   one of:
#'   \itemize{
#'     \item A \code{fixest} model with \code{i()} interaction coefficients.
#'     \item A data frame with columns \code{period}, \code{estimate},
#'       \code{se} (and optionally \code{ci_lower}, \code{ci_upper}).
#'     \item An \code{aggte} object from the \pkg{did} package.
#'   }
#' @param ref_period Integer. Reference period (normalized to 0). Default
#'   \code{-1}.
#' @param ci_type Character. Confidence interval type: \code{"pointwise"}
#'   (default) or \code{"simultaneous"} (wider, controls overall error rate).
#' @param conf_level Numeric. Confidence level. Default \code{0.95}.
#' @param show_pretrend_test Logical. Annotate with joint pre-trend test
#'   result. Default \code{TRUE}.
#' @param colors Character vector. Colors for each model. If \code{NULL},
#'   uses a built-in palette.
#' @param shapes Integer vector. Point shapes. Default: different shapes per
#'   model.
#' @param title Character. Plot title.
#' @param x_label Character. X-axis label. Default \code{"Period Relative to Treatment"}.
#' @param y_label Character. Y-axis label. Default \code{"Coefficient Estimate"}.
#' @param vline_color Character. Color of the treatment timing vertical line.
#'   Default \code{"gray30"}.
#' @param dodge_width Numeric. Horizontal dodge for multiple estimators.
#'   Default \code{0.3}.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' library(fixest)
#' data(base_stagg)
#'
#' # Single estimator (TWFE)
#' mod1 <- feols(y ~ i(time_to_treatment, ref = -1) | id + year, base_stagg)
#'
#' # Sun-Abraham
#' mod2 <- feols(y ~ sunab(year_treated, year) | id + year, base_stagg)
#'
#' did_event_study(
#'   models  = list("TWFE" = mod1, "Sun-Abraham" = mod2),
#'   ref_period = -1
#' )
#' }
#'
#' @importFrom stats qnorm coef vcov
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#'   geom_vline scale_color_manual labs theme_minimal position_dodge
#' @export
did_event_study <- function(models,
                            ref_period             = -1,
                            ci_type                = c("pointwise", "simultaneous"),
                            conf_level             = 0.95,
                            show_pretrend_test     = TRUE,
                            colors                 = NULL,
                            shapes                 = NULL,
                            title                  = "Event Study: Dynamic Treatment Effects",
                            x_label                = "Period Relative to Treatment",
                            y_label                = "Coefficient Estimate",
                            vline_color            = "gray30",
                            dodge_width            = 0.3) {

  ci_type <- match.arg(ci_type)
  z_crit  <- stats::qnorm(1 - (1 - conf_level) / 2)

  if (!is.list(models)) models <- list("Model" = models)
  if (is.null(names(models))) names(models) <- paste0("Model", seq_along(models))

  # --- Extract coefficients from each model ---
  extract_coefs <- function(obj, name) {
    if (is.data.frame(obj)) {
      req_cols <- c("period", "estimate", "se")
      if (!all(req_cols %in% names(obj))) {
        stop("Data frame for '", name, "' must have columns: period, estimate, se.",
             call. = FALSE)
      }
      df <- obj[, c("period", "estimate", "se")]
      df$ci_lower <- df$estimate - z_crit * df$se
      df$ci_upper <- df$estimate + z_crit * df$se
      return(df)
    }

    # fixest model
    if (inherits(obj, "fixest")) {
      ct <- tryCatch(fixest::coeftable(obj), error = function(e) NULL)
      if (is.null(ct)) return(NULL)
      cf_names <- rownames(ct)
      # Extract period numbers from coefficient names
      period_nums <- suppressWarnings(as.numeric(
        gsub(".*::(-?[0-9]+).*|.*:(-?[0-9]+)$|.*_(-?[0-9]+)$", "\\1\\2\\3",
             cf_names)
      ))
      valid <- !is.na(period_nums)
      if (sum(valid) == 0) return(NULL)
      df <- data.frame(
        period   = period_nums[valid],
        estimate = ct[valid, 1],
        se       = ct[valid, 2],
        ci_lower = ct[valid, 1] - z_crit * ct[valid, 2],
        ci_upper = ct[valid, 1] + z_crit * ct[valid, 2],
        stringsAsFactors = FALSE
      )
      # Add reference period row
      if (!ref_period %in% df$period) {
        df <- rbind(df, data.frame(
          period = ref_period, estimate = 0, se = 0,
          ci_lower = 0, ci_upper = 0
        ))
      }
      df <- df[order(df$period), ]
      return(df)
    }

    # did::aggte output
    if (inherits(obj, "aggte_obj")) {
      egt <- obj$egt
      att <- obj$att.egt
      se  <- obj$se.egt
      df  <- data.frame(
        period   = egt,
        estimate = att,
        se       = se,
        ci_lower = att - z_crit * se,
        ci_upper = att + z_crit * se,
        stringsAsFactors = FALSE
      )
      return(df)
    }

    warning("Unknown model type for '", name, "'. Skipping.", call. = FALSE)
    NULL
  }

  # Build combined data frame
  dfs <- lapply(names(models), function(nm) {
    df <- extract_coefs(models[[nm]], nm)
    if (!is.null(df)) df$model <- nm
    df
  })
  dfs <- Filter(Negate(is.null), dfs)

  if (length(dfs) == 0) stop("No valid models could be parsed.", call. = FALSE)

  plot_df <- do.call(rbind, dfs)
  plot_df$model <- factor(plot_df$model, levels = names(models))

  # --- Simultaneous CI adjustment (Bonferroni) ---
  if (ci_type == "simultaneous") {
    n_periods <- length(unique(plot_df$period))
    z_crit_sim <- stats::qnorm(1 - (1 - conf_level) / (2 * n_periods))
    plot_df$ci_lower <- plot_df$estimate - z_crit_sim * plot_df$se
    plot_df$ci_upper <- plot_df$estimate + z_crit_sim * plot_df$se
  }

  # --- Colors ---
  n_models <- length(unique(plot_df$model))
  if (is.null(colors)) {
    palette <- c("#2166AC", "#D73027", "#4DAF4A", "#FF7F00", "#984EA3",
                 "#A65628", "#F781BF", "#999999")
    colors <- palette[seq_len(n_models)]
  }
  names(colors) <- levels(plot_df$model)

  if (is.null(shapes)) {
    shapes <- (16:23)[seq_len(n_models)]
    names(shapes) <- levels(plot_df$model)
  }

  dodge <- ggplot2::position_dodge(width = dodge_width)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(
      x     = period,
      y     = estimate,
      color = model,
      shape = model,
      group = model
    )) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = ref_period + 0.5, linetype = "dotted",
                        color = vline_color) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2, position = dodge
    ) +
    ggplot2::geom_point(size = 3, position = dodge) +
    ggplot2::scale_color_manual(values = colors, name = "Estimator") +
    ggplot2::scale_shape_manual(values = shapes, name = "Estimator") +
    ggplot2::labs(
      x     = x_label,
      y     = y_label,
      title = title,
      caption = if (ci_type == "simultaneous") {
        sprintf("Simultaneous %.0f%% CI (Bonferroni-corrected)", conf_level * 100)
      } else {
        sprintf("Pointwise %.0f%% CI", conf_level * 100)
      }
    ) +
    ggplot2::theme_minimal(base_size = 12)

  # --- Pre-trend test annotation ---
  if (show_pretrend_test && n_models == 1) {
    obj <- models[[1]]
    if (inherits(obj, "fixest")) {
      pt <- tryCatch(test_pretrends(obj), error = function(e) NULL)
      if (!is.null(pt)) {
        p <- p + ggplot2::labs(
          subtitle = sprintf(
            "Pre-trends joint test: F = %.3f, p = %.3f",
            pt$joint_test$f_stat, pt$joint_test$p_value
          )
        )
      }
    }
  }

  p
}
