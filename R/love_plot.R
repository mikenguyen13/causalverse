#' Love Plot for Covariate Balance Visualization
#'
#' Creates a publication-ready Love plot (standardized mean differences plot)
#' comparing covariate balance before and after matching, weighting, or
#' any other balancing procedure. Supports multiple adjustment stages.
#'
#' @param data_pre Data frame or named numeric vector. Pre-adjustment covariate
#'   data (wide format) or pre-computed SMDs (named vector with covariate
#'   names as names).
#' @param data_post Data frame or named numeric vector. Post-adjustment data
#'   or SMDs. If \code{NULL} (default), only pre-adjustment shown.
#' @param treatment Character. Treatment variable name (only required when
#'   \code{data_pre} is a data frame).
#' @param weights_post Numeric vector. Analytical weights for post-adjustment
#'   (if \code{data_post} is a data frame).
#' @param covariates Character vector. Covariates to include. If \code{NULL},
#'   all numeric covariates are used.
#' @param threshold Numeric. The conventional balance threshold line (dashed).
#'   Default \code{0.1}.
#' @param abs_smd Logical. If \code{TRUE} (default), plot absolute SMD.
#' @param sort_by Character. How to sort covariates: \code{"pre"} (default,
#'   by pre-SMD), \code{"post"}, or \code{"name"}.
#' @param title Character. Plot title.
#' @param label_size Numeric. Font size for covariate labels. Default \code{3}.
#'
#' @return A ggplot2 Love plot.
#'
#' @references
#' Love, T. E. (2002). Displaying covariate balance after adjustment for
#' selection bias: An application in healthcare. \emph{Annual Conference of
#' the American Statistical Association.}
#'
#' Austin, P. C. (2009). Balance diagnostics for comparing the distribution
#' of baseline covariates between treatment groups in a propensity-score
#' matched sample. \emph{Statistics in Medicine}, 28(25), 3083–3107.
#'
#' @examples
#' data(lalonde, package = "MatchIt")
#' m <- MatchIt::matchit(treat ~ age + educ + re74 + re75,
#'                       data = lalonde, method = "nearest")
#' love_plot(
#'   data_pre   = lalonde,
#'   data_post  = MatchIt::match.data(m),
#'   treatment  = "treat",
#'   covariates = c("age", "educ", "re74", "re75"),
#'   weights_post = MatchIt::match.data(m)$weights
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_vline geom_point geom_line
#'   scale_color_manual scale_shape_manual labs theme_minimal theme
#'   element_text
#' @export
love_plot <- function(data_pre,
                      data_post    = NULL,
                      treatment    = NULL,
                      weights_post = NULL,
                      covariates   = NULL,
                      threshold    = 0.1,
                      abs_smd      = TRUE,
                      sort_by      = c("pre", "post", "name"),
                      title        = "Covariate Balance: Love Plot",
                      label_size   = 3) {

  sort_by <- match.arg(sort_by)

  # Helper to compute SMD from data frame
  compute_smd_df <- function(df, treat_var, vars, wts = NULL) {
    treat_vec <- as.integer(as.logical(df[[treat_var]]))
    if (is.null(wts)) wts <- rep(1, nrow(df))
    w0 <- wts[treat_vec == 0]
    w1 <- wts[treat_vec == 1]

    vapply(vars, function(v) {
      x0 <- df[[v]][treat_vec == 0]
      x1 <- df[[v]][treat_vec == 1]
      m0 <- stats::weighted.mean(x0, w0, na.rm = TRUE)
      m1 <- stats::weighted.mean(x1, w1, na.rm = TRUE)

      wsd <- function(xi, wi) {
        wi <- wi / sum(wi, na.rm = TRUE)
        mu <- sum(wi * xi, na.rm = TRUE)
        sqrt(sum(wi * (xi - mu)^2, na.rm = TRUE))
      }
      s0 <- wsd(x0, w0); s1 <- wsd(x1, w1)
      pooled_sd <- sqrt((s0^2 + s1^2) / 2)
      if (pooled_sd == 0) return(NA_real_)
      (m1 - m0) / pooled_sd
    }, numeric(1))
  }

  # Pre-adjustment SMDs
  if (is.numeric(data_pre) && !is.null(names(data_pre))) {
    smd_pre <- data_pre
    if (is.null(covariates)) covariates <- names(smd_pre)
  } else {
    if (is.null(treatment)) {
      stop("`treatment` is required when `data_pre` is a data frame.",
           call. = FALSE)
    }
    if (is.null(covariates)) {
      covariates <- setdiff(
        names(data_pre)[vapply(data_pre, is.numeric, logical(1))],
        treatment
      )
    }
    smd_pre <- compute_smd_df(data_pre, treatment, covariates)
  }

  # Post-adjustment SMDs
  smd_post <- NULL
  if (!is.null(data_post)) {
    if (is.numeric(data_post) && !is.null(names(data_post))) {
      smd_post <- data_post[covariates]
    } else {
      if (is.null(treatment)) {
        stop("`treatment` is required for post-adjustment data frame.",
             call. = FALSE)
      }
      smd_post <- compute_smd_df(data_post, treatment, covariates, weights_post)
    }
  }

  # Absolute SMD if requested
  if (abs_smd) {
    smd_pre  <- abs(smd_pre)
    if (!is.null(smd_post)) smd_post <- abs(smd_post)
  }

  # Build data frame for plotting
  df_plot <- data.frame(
    covariate = covariates,
    Pre       = smd_pre,
    stringsAsFactors = FALSE
  )
  if (!is.null(smd_post)) df_plot$Post <- smd_post

  # Sort
  if (sort_by == "pre") {
    df_plot <- df_plot[order(df_plot$Pre, decreasing = FALSE), ]
  } else if (sort_by == "post" && !is.null(smd_post)) {
    df_plot <- df_plot[order(df_plot$Post, decreasing = FALSE), ]
  } else {
    df_plot <- df_plot[order(df_plot$covariate), ]
  }
  df_plot$covariate <- factor(df_plot$covariate, levels = df_plot$covariate)

  # Reshape to long format
  df_long <- data.frame(
    covariate  = rep(df_plot$covariate, times = if (!is.null(smd_post)) 2 else 1),
    smd        = c(df_plot$Pre, if (!is.null(smd_post)) df_plot$Post),
    adjustment = c(
      rep("Before", nrow(df_plot)),
      if (!is.null(smd_post)) rep("After", nrow(df_plot))
    ),
    stringsAsFactors = FALSE
  )
  df_long$adjustment <- factor(df_long$adjustment,
                               levels = c("Before", "After"))

  x_label <- if (abs_smd) "Absolute Standardized Mean Difference" else
    "Standardized Mean Difference"

  p <- ggplot2::ggplot(df_long, ggplot2::aes(
      x     = smd,
      y     = covariate,
      color = adjustment,
      shape = adjustment
    )) +
    ggplot2::geom_vline(xintercept = 0, color = "gray70") +
    ggplot2::geom_vline(xintercept = threshold, linetype = "dashed",
                        color = "red", alpha = 0.6) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(
      values = c("Before" = "#D73027", "After" = "#2166AC"),
      name   = "Adjustment"
    ) +
    ggplot2::scale_shape_manual(
      values = c("Before" = 16, "After" = 17),
      name   = "Adjustment"
    ) +
    ggplot2::labs(
      x     = x_label,
      y     = NULL,
      title = title,
      caption = paste0("Dashed line at |SMD| = ", threshold)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.y   = ggplot2::element_text(size = label_size * 3),
      legend.position = "top"
    )

  # Connect pre/post for same covariate
  if (!is.null(smd_post)) {
    df_connect <- data.frame(
      covariate = df_plot$covariate,
      x_start   = df_plot$Pre,
      x_end     = df_plot$Post
    )
    p <- p +
      ggplot2::geom_segment(
        data = df_connect,
        ggplot2::aes(
          x = x_start, xend = x_end,
          y = covariate, yend = covariate
        ),
        color = "gray60", linewidth = 0.4,
        inherit.aes = FALSE
      )
  }

  p
}
