#' Universal Coefficient Plot
#'
#' Creates a publication-ready coefficient (forest) plot from any model or
#' list of models. Supports \code{lm}, \code{glm}, \code{fixest},
#' \code{ivreg}, and tidy data frames.
#'
#' @param models A model object, a named list of model objects, or a tidy
#'   data frame with columns \code{term}, \code{estimate}, \code{conf.low},
#'   \code{conf.high} (broom-style) or \code{ci_lower}, \code{ci_upper}
#'   (causalverse-style).
#' @param terms Character vector. Terms to include. If \code{NULL} (default),
#'   all non-intercept terms are shown.
#' @param term_labels Named character vector mapping original term names to
#'   display labels, e.g. \code{c(am = "Transmission (auto)")}.
#' @param model_labels Character vector. Display names for each model, used in
#'   the legend. Defaults to list element names or \code{"Model"}.
#' @param sort_by Character. Sort terms by \code{"estimate"} (default),
#'   \code{"name"}, or \code{"none"}.
#' @param add_zero_line Logical. Add a vertical reference line at zero.
#'   Default \code{TRUE}.
#' @param color_by Character. Color points by \code{"model"} (default) or
#'   \code{"significance"} (significant at 5 percent in a different color).
#' @param conf_level Numeric. Confidence level for intervals extracted from
#'   model objects. Default \code{0.95}.
#' @param dodge_width Numeric. Horizontal offset between multiple models.
#'   Default \code{0.4}.
#' @param title Character or \code{NULL}. Plot title.
#' @param xlab Character. X-axis label. Default \code{"Estimate (95 pct CI)"}.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' # Single lm model
#' mod <- lm(mpg ~ am + wt + hp, data = mtcars)
#' coef_plot(mod, terms = c("am", "wt", "hp"),
#'           term_labels = c(am = "Transmission (auto)",
#'                           wt = "Weight", hp = "Horsepower"))
#'
#' \dontrun{
#' # Multiple models
#' mod2 <- lm(mpg ~ am + wt + hp + disp, data = mtcars)
#' coef_plot(
#'   list(Parsimonious = mod, Full = mod2),
#'   terms = c("am", "wt", "hp"),
#'   color_by = "significance"
#' )
#' }
#'
#' @importFrom stats qnorm
#' @importFrom ggplot2 ggplot aes geom_pointrange geom_vline
#'   scale_color_manual labs coord_flip position_dodge theme
#'   element_text
#' @export
coef_plot <- function(models,
                      terms        = NULL,
                      term_labels  = NULL,
                      model_labels = NULL,
                      sort_by      = c("estimate", "name", "none"),
                      add_zero_line = TRUE,
                      color_by     = c("model", "significance"),
                      conf_level   = 0.95,
                      dodge_width  = 0.4,
                      title        = NULL,
                      xlab         = "Estimate (95 pct CI)") {

  sort_by  <- match.arg(sort_by)
  color_by <- match.arg(color_by)

  # ------------------------------------------------------------------
  # Normalise input to a named list
  # ------------------------------------------------------------------
  if (is.data.frame(models)) {
    models <- list(Model = models)
  } else if (!is.list(models) ||
             (!inherits(models, "list") && !is.null(attr(models, "class")))) {
    # Single model object
    nm     <- deparse(substitute(models))
    models <- stats::setNames(list(models), nm)
  } else {
    # List of models (or a single model wrapped in a list already)
    if (is.null(names(models))) {
      names(models) <- paste0("Model", seq_along(models))
    }
  }

  if (!is.null(model_labels)) {
    if (length(model_labels) != length(models)) {
      warning("`model_labels` length does not match number of models; ignoring.",
              call. = FALSE)
    } else {
      names(models) <- model_labels
    }
  }

  # ------------------------------------------------------------------
  # Extract tidy estimates from each model
  # ------------------------------------------------------------------
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

  extract_one <- function(obj, nm) {
    if (is.data.frame(obj)) {
      # Accept broom-style or causalverse-style columns
      if (all(c("term", "estimate", "conf.low", "conf.high") %in% names(obj))) {
        df <- obj[, c("term", "estimate", "conf.low", "conf.high")]
        names(df)[3:4] <- c("ci_lower", "ci_upper")
        if ("p.value" %in% names(obj)) df$p_value <- obj$p.value
        return(df)
      }
      if (all(c("term", "estimate", "ci_lower", "ci_upper") %in% names(obj))) {
        df <- obj[, c("term", "estimate", "ci_lower", "ci_upper")]
        if ("p_value" %in% names(obj)) df$p_value <- obj$p_value
        return(df)
      }
      stop("Data frame for model '", nm, "' must have columns ",
           "'term', 'estimate', and either ",
           "('conf.low', 'conf.high') or ('ci_lower', 'ci_upper').",
           call. = FALSE)
    }

    # Try tidy_causal first (causalverse-native), then broom fallback
    td <- tryCatch(
      tidy_causal(obj, conf_level = conf_level),
      error = function(e) NULL
    )

    if (is.null(td) && requireNamespace("broom", quietly = TRUE)) {
      td_b <- tryCatch(
        broom::tidy(obj, conf.int = TRUE, conf.level = conf_level),
        error = function(e) NULL
      )
      if (!is.null(td_b)) {
        td <- data.frame(
          term      = td_b$term,
          estimate  = td_b$estimate,
          ci_lower  = if ("conf.low"  %in% names(td_b)) td_b$conf.low
                      else td_b$estimate - z_crit * td_b$std.error,
          ci_upper  = if ("conf.high" %in% names(td_b)) td_b$conf.high
                      else td_b$estimate + z_crit * td_b$std.error,
          p_value   = if ("p.value" %in% names(td_b)) td_b$p.value else NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }

    if (is.null(td)) {
      stop("Cannot extract coefficients from model '", nm, "'. ",
           "Supply a tidy data frame or install the 'broom' package.",
           call. = FALSE)
    }

    # Rename causalverse columns if needed
    if ("ci_lower" %in% names(td) && !"ci_lower" %in% names(td)) {
      names(td)[names(td) == "ci_lower"] <- "ci_lower"
    }
    out <- data.frame(
      term     = td$term,
      estimate = td$estimate,
      ci_lower = if ("ci_lower" %in% names(td)) td$ci_lower
                 else td$estimate - z_crit * td$std_error,
      ci_upper = if ("ci_upper" %in% names(td)) td$ci_upper
                 else td$estimate + z_crit * td$std_error,
      p_value  = if ("p_value"  %in% names(td)) td$p_value else NA_real_,
      stringsAsFactors = FALSE
    )
    out
  }

  dfs <- lapply(names(models), function(nm) {
    df          <- extract_one(models[[nm]], nm)
    df$model_nm <- nm
    df
  })
  plot_df <- do.call(rbind, dfs)

  # ------------------------------------------------------------------
  # Filter terms
  # ------------------------------------------------------------------
  # Remove intercept-style terms by default
  intercept_patterns <- c("^\\(Intercept\\)$", "^Intercept$")
  is_intercept <- Reduce(`|`, lapply(intercept_patterns, function(pat)
    grepl(pat, plot_df$term)))

  if (!is.null(terms)) {
    plot_df <- plot_df[plot_df$term %in% terms, , drop = FALSE]
  } else {
    plot_df <- plot_df[!is_intercept, , drop = FALSE]
  }

  if (nrow(plot_df) == 0L) {
    stop("No terms remain after filtering. Check the `terms` argument.",
         call. = FALSE)
  }

  # ------------------------------------------------------------------
  # Apply term labels
  # ------------------------------------------------------------------
  plot_df$term_display <- plot_df$term
  if (!is.null(term_labels)) {
    idx <- match(plot_df$term, names(term_labels))
    valid <- !is.na(idx)
    plot_df$term_display[valid] <- unname(term_labels[idx[valid]])
  }

  # ------------------------------------------------------------------
  # Significance flag
  # ------------------------------------------------------------------
  plot_df$sig <- !is.na(plot_df$p_value) & (plot_df$p_value < 0.05)

  # ------------------------------------------------------------------
  # Sort
  # ------------------------------------------------------------------
  if (sort_by == "estimate") {
    # Sort by median estimate per term across models
    med_est  <- tapply(plot_df$estimate, plot_df$term_display, median,
                       na.rm = TRUE)
    ord      <- names(sort(med_est))
    plot_df$term_display <- factor(plot_df$term_display, levels = ord)
  } else if (sort_by == "name") {
    lvls <- sort(unique(plot_df$term_display))
    plot_df$term_display <- factor(plot_df$term_display, levels = lvls)
  } else {
    # "none": preserve existing order
    lvls <- unique(plot_df$term_display)
    plot_df$term_display <- factor(plot_df$term_display, levels = lvls)
  }

  plot_df$model_nm <- factor(plot_df$model_nm, levels = names(models))

  # ------------------------------------------------------------------
  # Color setup
  # ------------------------------------------------------------------
  n_models <- length(unique(plot_df$model_nm))
  palette  <- c("#2166AC", "#D73027", "#4DAF4A", "#FF7F00",
                "#984EA3", "#A65628", "#F781BF", "#999999")

  if (color_by == "model") {
    model_colors <- stats::setNames(
      palette[seq_len(n_models)],
      levels(plot_df$model_nm)
    )
    color_aes  <- ggplot2::aes(
      x     = estimate,
      xmin  = ci_lower,
      xmax  = ci_upper,
      y     = term_display,
      color = model_nm,
      group = model_nm
    )
    color_scale <- ggplot2::scale_color_manual(
      values = model_colors,
      name   = "Model"
    )
  } else {
    # color by significance
    sig_colors <- c("TRUE" = "#D73027", "FALSE" = "#2166AC")
    color_aes  <- ggplot2::aes(
      x     = estimate,
      xmin  = ci_lower,
      xmax  = ci_upper,
      y     = term_display,
      color = sig,
      group = model_nm
    )
    color_scale <- ggplot2::scale_color_manual(
      values = sig_colors,
      labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
      name   = "Significance"
    )
  }

  dodge <- ggplot2::position_dodge(width = dodge_width)

  # ------------------------------------------------------------------
  # Build plot
  # ------------------------------------------------------------------
  p <- ggplot2::ggplot(plot_df) +
    ggplot2::geom_pointrange(
      mapping  = color_aes,
      position = dodge,
      size     = 0.5,
      fatten   = 2
    ) +
    color_scale +
    ggplot2::labs(
      x     = xlab,
      y     = NULL,
      title = title
    ) +
    ama_theme(base_size = 12) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10)
    )

  if (add_zero_line) {
    p <- p + ggplot2::geom_vline(
      xintercept = 0,
      linetype   = "dashed",
      color      = "gray50"
    )
  }

  # Add model labels as shape if multiple models and coloring by significance
  if (n_models > 1L && color_by == "significance") {
    shape_vals <- stats::setNames(
      (15:22)[seq_len(n_models)],
      levels(plot_df$model_nm)
    )
    p <- p + ggplot2::aes(shape = model_nm) +
      ggplot2::scale_shape_manual(values = shape_vals, name = "Model")
  }

  p
}
