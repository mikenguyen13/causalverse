#' Publication-Ready Regression Discontinuity Plot
#'
#' Creates a publication-ready RD plot with optimal binning, local polynomial
#' fit, confidence bands, and a cutoff line. Wraps \code{rdrobust::rdplot}
#' from the \pkg{rdrobust} package with enhanced \pkg{ggplot2} aesthetics. When
#' \pkg{rdrobust} is not installed a simple local polynomial via \code{lm} is
#' used instead.
#'
#' @param y Numeric vector. Outcome variable.
#' @param x Numeric vector. Running variable. Values are shifted internally so
#'   the cutoff is at zero.
#' @param cutoff Numeric. RD cutoff value on the original scale. Default
#'   \code{0}.
#' @param nbins Integer or \code{NULL}. Number of bins per side. If \code{NULL}
#'   (default), MSE-optimal binning from \code{rdrobust::rdplot} is used.
#' @param poly_order Integer. Polynomial order for the smooth fit lines.
#'   Default \code{4}.
#' @param ci_shade Logical. Add confidence band shading around the polynomial
#'   fit. Default \code{TRUE}.
#' @param title Character or \code{NULL}. Plot title.
#' @param xlab Character. X-axis label. Default \code{"Running Variable"}.
#' @param ylab Character. Y-axis label. Default \code{"Outcome"}.
#' @param cutoff_color Character. Color of the vertical cutoff line.
#'   Default \code{"red"}.
#' @param color_sides Logical. Use different colors for the control (left) and
#'   treated (right) sides. Default \code{TRUE}.
#' @param rdplot_args List. Additional named arguments forwarded to
#'   \code{rdrobust::rdplot} (ignored when \pkg{rdrobust} is not available).
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' library(rdrobust)
#' data(rdrobust_RDsenate)
#' rd_plot(
#'   y      = rdrobust_RDsenate$vote,
#'   x      = rdrobust_RDsenate$margin,
#'   cutoff = 0,
#'   title  = "RD: Senate Vote Share",
#'   xlab   = "Democratic Vote Share Margin",
#'   ylab   = "Democratic Vote Share (next election)"
#' )
#' }
#'
#' @references
#' Calonico, S., Cattaneo, M. D., and Titiunik, R. (2015). "rdrobust: An R
#' Package for Robust Nonparametric Inference in Regression-Discontinuity
#' Designs." \emph{R Journal}, 7(1), 38-51.
#'
#' @importFrom stats lm predict
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_ribbon
#'   geom_vline scale_color_manual scale_fill_manual labs
#' @export
rd_plot <- function(y,
                    x,
                    cutoff       = 0,
                    nbins        = NULL,
                    poly_order   = 4L,
                    ci_shade     = TRUE,
                    title        = NULL,
                    xlab         = "Running Variable",
                    ylab         = "Outcome",
                    cutoff_color = "red",
                    color_sides  = TRUE,
                    rdplot_args  = list()) {

  # ------------------------------------------------------------------
  # Input checks
  # ------------------------------------------------------------------
  if (!is.numeric(y) || !is.numeric(x)) {
    stop("`y` and `x` must be numeric vectors.", call. = FALSE)
  }
  if (length(y) != length(x)) {
    stop("`y` and `x` must have the same length.", call. = FALSE)
  }
  keep <- stats::complete.cases(y, x)
  y    <- y[keep]
  x    <- x[keep]
  xc   <- x - cutoff   # center at cutoff

  n_left  <- sum(xc < 0)
  n_right <- sum(xc >= 0)
  n_total <- length(xc)

  if (n_left < 5L || n_right < 5L) {
    stop("Too few observations on one or both sides of the cutoff.",
         call. = FALSE)
  }

  # ------------------------------------------------------------------
  # Obtain bin means and polynomial fit
  # ------------------------------------------------------------------
  bins_df <- NULL
  fit_df  <- NULL

  if (requireNamespace("rdrobust", quietly = TRUE)) {
    rdrobust_res <- .rd_bins_from_rdrobust(y, xc, cutoff, nbins,
                                            poly_order, rdplot_args)
    if (!is.null(rdrobust_res)) {
      bins_df <- rdrobust_res$bins
      fit_df  <- rdrobust_res$fit
    }
  }

  if (is.null(bins_df)) {
    bins_df <- .rd_bins_manual(y, xc, nbins)
  }

  if (is.null(fit_df)) {
    fit_df <- .rd_poly_fit(y, xc, poly_order, ci_shade)
  }

  # ------------------------------------------------------------------
  # Side labels for color mapping
  # ------------------------------------------------------------------
  bins_df$side <- ifelse(bins_df$x_mid < 0, "Control", "Treated")
  fit_df$side  <- ifelse(fit_df$x_fit  < 0, "Control", "Treated")

  if (color_sides) {
    side_colors <- c(Control = "#2166AC", Treated = "#D73027")
    side_fills  <- c(Control = "#A6CEE3", Treated = "#FABEBE")
  } else {
    side_colors <- c(Control = "black", Treated = "black")
    side_fills  <- c(Control = "gray80", Treated = "gray80")
  }

  # ------------------------------------------------------------------
  # Build ggplot
  # ------------------------------------------------------------------
  p <- ggplot2::ggplot()

  # CI ribbon
  if (ci_shade && all(c("ci_low", "ci_high") %in% names(fit_df))) {
    # Split ribbon by side to avoid spanning the cutoff
    p <- p +
      ggplot2::geom_ribbon(
        data = fit_df[fit_df$side == "Control", ],
        ggplot2::aes(x = x_fit, ymin = ci_low, ymax = ci_high),
        fill  = side_fills["Control"],
        alpha = 0.4
      ) +
      ggplot2::geom_ribbon(
        data = fit_df[fit_df$side == "Treated", ],
        ggplot2::aes(x = x_fit, ymin = ci_low, ymax = ci_high),
        fill  = side_fills["Treated"],
        alpha = 0.4
      )
  }

  # Polynomial fit lines (split at cutoff)
  p <- p +
    ggplot2::geom_line(
      data = fit_df[fit_df$side == "Control", ],
      ggplot2::aes(x = x_fit, y = y_fit, color = side),
      linewidth = 1.0
    ) +
    ggplot2::geom_line(
      data = fit_df[fit_df$side == "Treated", ],
      ggplot2::aes(x = x_fit, y = y_fit, color = side),
      linewidth = 1.0
    )

  # Bin means (dots)
  ci_cols_present <- all(c("ci_l", "ci_r") %in% names(bins_df))
  if (ci_cols_present) {
    p <- p +
      ggplot2::geom_errorbar(
        data  = bins_df,
        ggplot2::aes(x = x_mid, ymin = ci_l, ymax = ci_r, color = side),
        width = 0, alpha = 0.5, linewidth = 0.5
      )
  }

  p <- p +
    ggplot2::geom_point(
      data  = bins_df,
      ggplot2::aes(x = x_mid, y = y_mean, color = side),
      size  = 1.8,
      alpha = 0.85
    )

  # Cutoff line
  p <- p +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype   = "dashed",
      color      = cutoff_color,
      linewidth  = 0.8
    )

  # Color scales
  p <- p +
    ggplot2::scale_color_manual(
      values = side_colors,
      name   = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = side_fills,
      name   = NULL,
      guide  = "none"
    )

  # Labels
  subtitle_txt <- sprintf(
    "N = %d (left: %d, right: %d) | Cutoff = %g | Polynomial order = %d",
    n_total, n_left, n_right, cutoff, poly_order
  )

  p <- p +
    ggplot2::labs(
      x        = xlab,
      y        = ylab,
      title    = title,
      subtitle = subtitle_txt
    ) +
    ama_theme(base_size = 12)

  if (!color_sides) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p
}


# ------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------

#' @keywords internal
.rd_bins_from_rdrobust <- function(y, xc, cutoff, nbins, poly_order,
                                    rdplot_args) {
  tryCatch({
    args <- c(
      list(y = y, x = xc, c = 0, p = poly_order),
      if (!is.null(nbins)) list(nbins = c(nbins, nbins)) else list(),
      rdplot_args
    )
    # Suppress rdplot output/device
    rp <- suppressMessages(
      suppressWarnings(do.call(rdrobust::rdplot, c(args, list(hide = TRUE))))
    )

    vb <- rp$vars_bins
    if (is.null(vb)) return(NULL)

    # rdplot bin column names may vary across package versions
    x_col   <- intersect(c("rdplot_mean_x", "x"), names(vb))[1L]
    y_col   <- intersect(c("rdplot_mean_y", "y"), names(vb))[1L]
    cil_col <- intersect(c("rdplot_ci_l", "ci_l"), names(vb))[1L]
    cir_col <- intersect(c("rdplot_ci_r", "ci_r"), names(vb))[1L]

    if (is.na(x_col) || is.na(y_col)) return(NULL)

    bins <- data.frame(
      x_mid  = vb[[x_col]],
      y_mean = vb[[y_col]],
      stringsAsFactors = FALSE
    )
    if (!is.na(cil_col) && !is.na(cir_col)) {
      bins$ci_l <- vb[[cil_col]]
      bins$ci_r <- vb[[cir_col]]
    }

    # Polynomial fit from rdplot
    vp  <- rp$vars_poly
    fit <- NULL
    if (!is.null(vp)) {
      xp_col  <- intersect(c("rdplot_x", "x"), names(vp))[1L]
      ypl_col <- intersect(c("rdplot_y_l", "y_l"), names(vp))[1L]
      ypr_col <- intersect(c("rdplot_y_r", "y_r"), names(vp))[1L]
      if (!is.na(xp_col) && !is.na(ypl_col)) {
        left_mask  <- vp[[xp_col]] < 0
        right_mask <- vp[[xp_col]] >= 0
        fit <- rbind(
          data.frame(x_fit = vp[[xp_col]][left_mask],
                     y_fit = vp[[ypl_col]][left_mask],
                     stringsAsFactors = FALSE),
          data.frame(x_fit = vp[[xp_col]][right_mask],
                     y_fit = if (!is.na(ypr_col))
                               vp[[ypr_col]][right_mask]
                             else
                               vp[[ypl_col]][right_mask],
                     stringsAsFactors = FALSE)
        )
      }
    }

    list(bins = bins, fit = fit)
  }, error = function(e) NULL)
}


#' @keywords internal
.rd_bins_manual <- function(y, xc, nbins) {
  n_bins_side <- if (!is.null(nbins)) as.integer(nbins) else
    max(10L, min(40L, as.integer(sqrt(length(y) / 2))))

  left_idx  <- xc < 0
  right_idx <- xc >= 0

  bin_side <- function(y_s, x_s, n_b) {
    if (length(y_s) == 0L) return(NULL)
    breaks <- seq(min(x_s), max(x_s), length.out = n_b + 1L)
    grp    <- findInterval(x_s, breaks, rightmost.closed = TRUE)
    grp    <- pmin(grp, n_b)
    do.call(rbind, lapply(seq_len(n_b), function(g) {
      idx <- grp == g
      if (sum(idx) == 0L) return(NULL)
      ymid <- mean(y_s[idx])
      xmid <- mean(x_s[idx])
      n_g  <- sum(idx)
      se   <- if (n_g > 1L) stats::sd(y_s[idx]) / sqrt(n_g) else NA_real_
      data.frame(
        x_mid  = xmid,
        y_mean = ymid,
        ci_l   = ymid - 1.96 * se,
        ci_r   = ymid + 1.96 * se,
        stringsAsFactors = FALSE
      )
    }))
  }

  rbind(
    bin_side(y[left_idx],  xc[left_idx],  n_bins_side),
    bin_side(y[right_idx], xc[right_idx], n_bins_side)
  )
}


#' @keywords internal
.rd_poly_fit <- function(y, xc, poly_order, ci_shade) {

  # Check if rdrobust already provided a fit (via attribute)
  # (Not used here since bins and fit are handled separately in caller,
  #  but we keep this for direct construction.)

  fit_side <- function(y_s, x_s, side_label) {
    if (length(y_s) < poly_order + 2L) return(NULL)
    poly_terms <- stats::poly(x_s, degree = min(poly_order, length(unique(x_s)) - 1L),
                       raw = TRUE)
    fit_df_s   <- as.data.frame(poly_terms)
    fit_df_s$y <- y_s
    tryCatch({
      mod     <- stats::lm(y ~ ., data = fit_df_s)
      x_grid  <- seq(min(x_s), max(x_s), length.out = 200L)
      pg      <- stats::poly(x_grid,
                      degree = min(poly_order, length(unique(x_s)) - 1L),
                      raw = TRUE)
      new_df  <- as.data.frame(pg)
      pred    <- stats::predict(mod, newdata = new_df,
                                interval = if (ci_shade) "confidence" else "none")
      if (ci_shade && is.matrix(pred)) {
        data.frame(x_fit  = x_grid,
                   y_fit  = pred[, "fit"],
                   ci_low = pred[, "lwr"],
                   ci_high= pred[, "upr"],
                   stringsAsFactors = FALSE)
      } else {
        data.frame(x_fit = x_grid,
                   y_fit = if (is.matrix(pred)) pred[, "fit"] else as.numeric(pred),
                   stringsAsFactors = FALSE)
      }
    }, error = function(e) NULL)
  }

  left_idx  <- xc < 0
  right_idx <- xc >= 0

  left_fit  <- fit_side(y[left_idx],  xc[left_idx],  "Control")
  right_fit <- fit_side(y[right_idx], xc[right_idx], "Treated")

  do.call(rbind, Filter(Negate(is.null), list(left_fit, right_fit)))
}
