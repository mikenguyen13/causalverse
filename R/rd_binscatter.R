#' Binned Scatter Plot for Regression Discontinuity Analysis
#'
#' Creates a customisable binned scatter plot for regression discontinuity
#' (RD) analysis. The running variable is split into equal-width bins on each
#' side of the cutoff, bin means are computed, and a polynomial of chosen
#' order is fitted separately on each side. Optional 95% confidence bands are
#' shown. The cutoff is marked with a vertical line and a visible gap between
#' the two sides. No dependency on \pkg{rdrobust} is required.
#'
#' @title Binned Scatter Plot for RD Analysis
#'
#' @description Produces a publication-ready RD plot that clearly visualises
#'   the discontinuity at the cutoff by displaying bin means and a polynomial
#'   fit on each side, with an optional confidence ribbon.
#'
#' @param y Numeric vector. Outcome variable.
#' @param x Numeric vector. Running variable (same length as `y`).
#' @param cutoff Numeric. The RD cutoff value. Default `0`.
#' @param nbins Integer. Number of bins on each side of the cutoff. Default
#'   `20`.
#' @param poly_order Integer. Degree of the polynomial fit (1 = linear, 2 =
#'   quadratic). Default `1`.
#' @param show_ci Logical. Show 95% confidence ribbon around the polynomial
#'   fit. Default `TRUE`.
#' @param xlab Character. x-axis label. Default `"Running Variable"`.
#' @param ylab Character. y-axis label. Default `"Outcome"`.
#' @param title Character or `NULL`. Plot title.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' set.seed(99)
#' n <- 1000
#' x <- runif(n, -1, 1)
#' y <- 1.5 + 0.8 * x + 2.0 * (x >= 0) + rnorm(n, 0, 0.5)
#'
#' rd_binscatter(
#'   y         = y,
#'   x         = x,
#'   cutoff    = 0,
#'   nbins     = 20,
#'   poly_order = 1,
#'   title     = "RD Binned Scatter: Simulated Data"
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#'   geom_vline scale_color_manual scale_fill_manual labs theme
#' @importFrom stats lm predict sd
#' @export
rd_binscatter <- function(y,
                           x,
                           cutoff     = 0,
                           nbins      = 20,
                           poly_order = 1,
                           show_ci    = TRUE,
                           xlab       = "Running Variable",
                           ylab       = "Outcome",
                           title      = NULL) {

  stopifnot(length(y) == length(x))
  keep <- !is.na(x) & !is.na(y)
  x    <- x[keep]
  y    <- y[keep]

  if (length(x) < 10) stop("Too few observations after removing NAs.")

  # ---- Bin each side separately --------------------------------------------
  x_left  <- x[x <  cutoff]
  y_left  <- y[x <  cutoff]
  x_right <- x[x >= cutoff]
  y_right <- y[x >= cutoff]

  bin_side <- function(xv, yv, side_label) {
    brks <- seq(min(xv), max(xv), length.out = nbins + 1)
    bidx <- cut(xv, breaks = brks, include.lowest = TRUE, labels = FALSE)
    mids <- (brks[-length(brks)] + brks[-1]) / 2
    do.call(rbind, lapply(seq_len(nbins), function(b) {
      in_b <- which(bidx == b)
      if (length(in_b) == 0) return(NULL)
      data.frame(
        x_bin  = mids[b],
        y_mean = mean(yv[in_b]),
        side   = side_label,
        n      = length(in_b),
        stringsAsFactors = FALSE
      )
    }))
  }

  bins_left  <- bin_side(x_left,  y_left,  "Left")
  bins_right <- bin_side(x_right, y_right, "Right")
  bin_df     <- do.call(rbind, Filter(Negate(is.null), list(bins_left, bins_right)))

  # ---- Polynomial fit on raw data ------------------------------------------
  fit_side <- function(xv, yv, side_label) {
    if (length(xv) < (poly_order + 2)) return(NULL)
    df_fit  <- data.frame(y = yv, x = xv)
    fml     <- stats::as.formula(paste0("y ~ poly(x, ", poly_order, ", raw = TRUE)"))
    fit     <- stats::lm(fml, data = df_fit)

    x_seq   <- seq(min(xv), max(xv), length.out = 200)
    nd      <- data.frame(x = x_seq)
    pred    <- stats::predict(fit, newdata = nd, interval = "confidence")
    data.frame(
      x_fit    = x_seq,
      y_fit    = pred[, "fit"],
      ci_low_f = pred[, "lwr"],
      ci_hi_f  = pred[, "upr"],
      side     = side_label,
      stringsAsFactors = FALSE
    )
  }

  fit_left  <- fit_side(x_left,  y_left,  "Left")
  fit_right <- fit_side(x_right, y_right, "Right")
  fit_df    <- do.call(rbind, Filter(Negate(is.null), list(fit_left, fit_right)))

  # ---- Colors --------------------------------------------------------------
  side_colors <- c("Left"  = "#4575b4", "Right" = "#d73027")
  side_fills  <- c("Left"  = "#4575b4", "Right" = "#d73027")

  plot_title <- if (!is.null(title)) title else "Regression Discontinuity Plot"

  # ---- Build plot ----------------------------------------------------------
  p <- ggplot2::ggplot() +
    # CI ribbons for polynomial fit
    {
      if (show_ci && !is.null(fit_df)) {
        ggplot2::geom_ribbon(
          data = fit_df,
          ggplot2::aes(x = x_fit, ymin = ci_low_f, ymax = ci_hi_f,
                       fill = side),
          alpha = 0.18, color = NA)
      }
    } +
    # Polynomial fit lines
    {
      if (!is.null(fit_df)) {
        ggplot2::geom_line(
          data = fit_df,
          ggplot2::aes(x = x_fit, y = y_fit, color = side),
          linewidth = 1.2)
      }
    } +
    # Bin means
    ggplot2::geom_point(
      data = bin_df,
      ggplot2::aes(x = x_bin, y = y_mean, color = side),
      size = 2.5, alpha = 0.85) +
    # Cutoff line
    ggplot2::geom_vline(xintercept = cutoff, linetype = "dashed",
                        color = "grey30", linewidth = 0.8) +
    ggplot2::scale_color_manual(values = side_colors, name = "Side of Cutoff") +
    ggplot2::scale_fill_manual(values  = side_fills,  name = "Side of Cutoff") +
    ggplot2::labs(
      title    = plot_title,
      subtitle = paste0("Poly order: ", poly_order, " | Bins per side: ", nbins,
                        " | Cutoff: ", cutoff),
      x        = xlab,
      y        = ylab
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "bottom")

  p
}
