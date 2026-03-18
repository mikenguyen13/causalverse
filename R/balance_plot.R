#' Comprehensive Balance Plot Suite
#'
#' Produces a multi-panel diagnostic figure comparing covariate distributions
#' between treatment and control groups. Combines a standardized mean difference
#' (SMD) dot plot, density overlays, and (optionally) a variance ratio panel.
#' Useful for assessing balance before and after matching or weighting.
#'
#' @param data A data frame.
#' @param treatment Character. Name of the binary treatment variable (0/1).
#' @param covariates Character vector. Names of covariates to plot. Defaults
#'   to all numeric variables except \code{treatment}.
#' @param data_adj A second data frame (e.g., post-matching) to overlay as
#'   "adjusted" estimates. Optional.
#' @param weights Numeric vector. Case weights for the adjusted sample
#'   (alternative to \code{data_adj}). Length must equal \code{nrow(data)}.
#' @param threshold Numeric. SMD threshold line. Default \code{0.1}.
#' @param var_labels Named character vector. Display names for covariates.
#' @param show_density Logical. Add density overlay panels. Default \code{TRUE}.
#' @param show_variance_ratio Logical. Add variance ratio panel. Default \code{FALSE}.
#' @param title Character. Overall plot title.
#'
#' @return A ggplot2 object (or patchwork figure if multiple panels).
#'
#' @examples
#' set.seed(42)
#' n <- 300
#' df <- data.frame(
#'   treat  = rbinom(n, 1, 0.5),
#'   age    = rnorm(n, 40, 10),
#'   income = rnorm(n, 50000, 15000),
#'   female = rbinom(n, 1, 0.5)
#' )
#' # Introduce imbalance
#' df$age[df$treat == 1] <- df$age[df$treat == 1] + 5
#'
#' balance_plot(df, treatment = "treat",
#'              covariates = c("age", "income", "female"))
#'
#' @export
balance_plot <- function(data,
                          treatment,
                          covariates         = NULL,
                          data_adj           = NULL,
                          weights            = NULL,
                          threshold          = 0.1,
                          var_labels         = NULL,
                          show_density       = TRUE,
                          show_variance_ratio= FALSE,
                          title              = "Covariate Balance") {

  if (is.null(covariates)) {
    covariates <- setdiff(names(data)[sapply(data, is.numeric)], treatment)
  }
  covariates <- intersect(covariates, names(data))
  if (length(covariates) == 0) stop("No numeric covariates found.")

  W <- data[[treatment]]

  # Helper: compute SMD
  smd_calc <- function(x, w, wts = NULL) {
    if (!is.null(wts)) {
      m1 <- stats::weighted.mean(x[w == 1], wts[w == 1], na.rm = TRUE)
      m0 <- stats::weighted.mean(x[w == 0], wts[w == 0], na.rm = TRUE)
      v1 <- stats::weighted.mean((x[w == 1] - m1)^2, wts[w == 1], na.rm = TRUE)
      v0 <- stats::weighted.mean((x[w == 0] - m0)^2, wts[w == 0], na.rm = TRUE)
    } else {
      m1 <- mean(x[w == 1], na.rm = TRUE)
      m0 <- mean(x[w == 0], na.rm = TRUE)
      v1 <- var(x[w == 1], na.rm = TRUE)
      v0 <- var(x[w == 0], na.rm = TRUE)
    }
    pooled_sd <- sqrt((v1 + v0) / 2)
    if (is.na(pooled_sd) || pooled_sd == 0) return(0)
    (m1 - m0) / pooled_sd
  }

  # Build SMD data
  smds <- lapply(covariates, function(v) {
    xv <- data[[v]]
    if (!is.numeric(xv)) xv <- as.numeric(as.factor(xv))

    smd_unadj <- smd_calc(xv, W)

    smd_adj <- NA_real_
    if (!is.null(data_adj)) {
      Wadj <- data_adj[[treatment]]
      xadj <- data_adj[[v]]
      if (!is.numeric(xadj)) xadj <- as.numeric(as.factor(xadj))
      smd_adj <- smd_calc(xadj, Wadj)
    } else if (!is.null(weights)) {
      smd_adj <- smd_calc(xv, W, wts = weights)
    }
    data.frame(
      covariate  = v,
      label      = if (!is.null(var_labels) && v %in% names(var_labels)) var_labels[[v]] else v,
      smd_unadj  = smd_unadj,
      smd_adj    = smd_adj,
      stringsAsFactors = FALSE
    )
  })
  smd_df <- do.call(rbind, smds)
  smd_df <- smd_df[order(abs(smd_df$smd_unadj)), ]
  smd_df$label <- factor(smd_df$label, levels = smd_df$label)

  has_adj <- !is.null(data_adj) || !is.null(weights)

  # Pivot long
  plot_df <- smd_df
  if (has_adj) {
    long_df <- tidyr::pivot_longer(
      plot_df[, c("label", "smd_unadj", "smd_adj")],
      cols      = c("smd_unadj", "smd_adj"),
      names_to  = "adjustment",
      values_to = "smd"
    )
    long_df$adjustment <- ifelse(long_df$adjustment == "smd_unadj",
                                 "Unadjusted", "Adjusted")
    long_df$adjustment <- factor(long_df$adjustment,
                                 levels = c("Unadjusted", "Adjusted"))
  } else {
    long_df <- data.frame(
      label      = plot_df$label,
      smd        = plot_df$smd_unadj,
      adjustment = factor("Unadjusted"),
      stringsAsFactors = FALSE
    )
  }
  long_df <- long_df[!is.na(long_df$smd), ]

  # --- SMD dot plot ---
  colors <- c("Unadjusted" = "#d73027", "Adjusted" = "#4575b4")

  p_smd <- ggplot2::ggplot(long_df,
      ggplot2::aes(x = smd, y = label,
                   color = adjustment, shape = adjustment)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey30") +
    ggplot2::geom_vline(xintercept = c(-threshold, threshold),
                        linetype = "dashed", color = "grey60") +
    ggplot2::geom_point(size = 3, alpha = 0.9) +
    ggplot2::scale_color_manual(values = colors, name = NULL) +
    ggplot2::scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 17),
                                name = NULL) +
    ggplot2::labs(
      title = title,
      x     = "Standardized Mean Difference",
      y     = NULL
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "bottom")

  if (!show_density) return(p_smd)

  # --- Density overlays for top imbalanced variables ---
  top_vars <- smd_df$covariate[order(abs(smd_df$smd_unadj), decreasing = TRUE)]
  top_vars <- utils::head(top_vars, min(4, length(top_vars)))

  density_plots <- lapply(top_vars, function(v) {
    xv <- data[[v]]
    if (!is.numeric(xv) || length(unique(xv)) < 3) return(NULL)

    dens_df <- data.frame(x = xv, group = factor(W, labels = c("Control", "Treated")))

    lbl <- if (!is.null(var_labels) && v %in% names(var_labels)) var_labels[[v]] else v

    ggplot2::ggplot(dens_df, ggplot2::aes(x = x, fill = group, color = group)) +
      ggplot2::geom_density(alpha = 0.35) +
      ggplot2::scale_fill_manual(
        values = c("Control" = "#d73027", "Treated" = "#4575b4"), name = NULL) +
      ggplot2::scale_color_manual(
        values = c("Control" = "#d73027", "Treated" = "#4575b4"), name = NULL) +
      ggplot2::labs(x = lbl, y = "Density") +
      causalverse::ama_theme() +
      ggplot2::theme(legend.position = "bottom")
  })
  density_plots <- Filter(Negate(is.null), density_plots)

  if (length(density_plots) == 0) return(p_smd)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    dens_panel <- patchwork::wrap_plots(density_plots, ncol = 2)
    combined   <- patchwork::wrap_plots(p_smd, dens_panel,
                                         ncol = 1, heights = c(1.5, 1))
    return(combined)
  }
  p_smd
}
