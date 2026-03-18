#' Sensitivity Analysis for Instrumental Variables: Exclusion Restriction
#'
#' Assesses the sensitivity of 2SLS estimates to partial violations of the
#' exclusion restriction. Computes the "breakdown point" -- how strong the
#' direct effect of the instrument on the outcome (plausibly through omitted
#' channels) must be to explain away the IV estimate.
#'
#' @param data A data frame.
#' @param outcome Character. Outcome variable name.
#' @param treatment Character. Endogenous treatment variable name.
#' @param instrument Character or character vector. Instrument name(s).
#' @param covariates Character vector. Exogenous covariates. Default
#'   \code{NULL}.
#' @param delta_seq Numeric vector. Sequence of direct effect strengths (as
#'   fraction of the first-stage coefficient) to evaluate. Default
#'   \code{seq(0, 1, by = 0.05)}.
#' @param conf_level Numeric. Confidence level. Default \code{0.95}.
#' @param plot Logical. Whether to produce a sensitivity plot. Default
#'   \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{iv_estimate}{Numeric. Original 2SLS estimate.}
#'     \item{first_stage_coef}{Numeric. First-stage coefficient on instrument.}
#'     \item{breakdown_delta}{Numeric. Value of \eqn{\delta} at which the
#'       bias-adjusted 95% CI includes zero.}
#'     \item{sensitivity_df}{Data frame with \code{delta}, adjusted estimate,
#'       CI bounds.}
#'     \item{plot}{ggplot2 sensitivity plot.}
#'   }
#'
#' @details
#' Following Conley et al. (2012), suppose the true model is
#' \eqn{Y = \tau D + \delta Z + X'\beta + \varepsilon} where \eqn{Z} is the
#' instrument. Under standard 2SLS (assuming \eqn{\delta = 0}), the estimate
#' is consistent. For a given \eqn{\delta}, the bias in the 2SLS estimator
#' is \eqn{-\delta / \pi} where \eqn{\pi} is the first-stage coefficient.
#' This function sweeps over \eqn{\delta} to show how the estimate changes.
#'
#' @references
#' Conley, T. G., Hansen, C. B., & Rossi, P. E. (2012). Plausibly exogenous.
#' \emph{Review of Economics and Statistics}, 94(1), 260–272.
#'
#' @examples
#' \dontrun{
#' library(ivreg)
#' # Card (1995) example
#' result <- iv_sensitivity(
#'   data       = card_data,
#'   outcome    = "log_wage",
#'   treatment  = "educ",
#'   instrument = "proximity_college",
#'   covariates = c("exper", "expersq", "black", "south")
#' )
#' result$plot
#' result$breakdown_delta
#' }
#'
#' @importFrom stats lm coef vcov qnorm as.formula
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_hline
#'   labs theme_minimal
#' @export
iv_sensitivity <- function(data,
                           outcome,
                           treatment,
                           instrument,
                           covariates  = NULL,
                           delta_seq   = seq(0, 1, by = 0.05),
                           conf_level  = 0.95,
                           plot        = TRUE) {

  req_cols <- c(outcome, treatment, instrument, covariates)
  missing  <- setdiff(req_cols, names(data))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)

  # First stage
  rhs_fs  <- paste(c(instrument, covariates), collapse = " + ")
  fs_form <- stats::as.formula(paste(treatment, "~", rhs_fs))
  fs_mod  <- stats::lm(fs_form, data = data)
  pi_hat  <- stats::coef(fs_mod)[instrument[1]]

  # 2SLS via control function / ivreg
  if (requireNamespace("ivreg", quietly = TRUE)) {
    iv_form <- stats::as.formula(
      paste0(
        outcome, " ~ ", treatment,
        if (!is.null(covariates)) paste0(" + ", paste(covariates, collapse = " + ")) else "",
        " | ", paste(c(instrument, covariates), collapse = " + ")
      )
    )
    iv_mod  <- ivreg::ivreg(iv_form, data = data)
    iv_est  <- stats::coef(iv_mod)[treatment]
    iv_se   <- sqrt(stats::vcov(iv_mod)[treatment, treatment])
  } else {
    # Manual 2SLS
    data$.D_hat <- stats::predict(fs_mod)
    rhs_ss <- paste(c(".D_hat", covariates), collapse = " + ")
    ss_mod <- stats::lm(
      stats::as.formula(paste(outcome, "~", rhs_ss)),
      data = data
    )
    iv_est <- stats::coef(ss_mod)[".D_hat"]
    iv_se  <- sqrt(stats::vcov(ss_mod)[".D_hat", ".D_hat"])
    data$.D_hat <- NULL
  }

  # --- Sensitivity sweep: bias = -delta * (var(Z) / cov(Z, D)) ---
  # More simply: bias ≈ -delta / pi_hat (per unit of Z direct effect)
  # delta is the direct effect of Z on Y (in same units as Y)
  # We parameterize as fraction of pi_hat for scaling

  sens_rows <- lapply(delta_seq, function(delta) {
    # bias = delta * (1/pi) if delta = direct effect per unit instrument
    # Here we parameterize delta as coefficient of Z in outcome eq
    bias  <- if (pi_hat != 0) delta / pi_hat else 0
    adj   <- iv_est - bias
    data.frame(
      delta     = delta,
      estimate  = adj,
      ci_lower  = adj - z_crit * iv_se,
      ci_upper  = adj + z_crit * iv_se
    )
  })
  sens_df <- do.call(rbind, sens_rows)

  # Breakdown point: smallest delta where CI includes 0
  breakdown <- delta_seq[
    which.max(!is.na(sens_df$ci_lower) &
              sens_df$ci_lower <= 0 & sens_df$ci_upper >= 0)
  ]
  if (length(breakdown) == 0) breakdown <- NA

  out <- list(
    iv_estimate        = iv_est,
    iv_se              = iv_se,
    first_stage_coef   = pi_hat,
    breakdown_delta    = breakdown,
    sensitivity_df     = sens_df,
    plot               = NULL
  )

  if (plot) {
    p <- ggplot2::ggplot(sens_df, ggplot2::aes(x = delta, y = estimate)) +
      ggplot2::geom_hline(yintercept = 0, color = "gray50") +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
        fill = "#4393C3", alpha = 0.25
      ) +
      ggplot2::geom_line(color = "#2166AC", linewidth = 1) +
      ggplot2::labs(
        x     = expression(paste(delta, " (Direct Effect of Z on Y)")),
        y     = "Bias-Adjusted 2SLS Estimate",
        title = "IV Sensitivity Analysis: Exclusion Restriction",
        subtitle = sprintf(
          "Original IV estimate: %.4f | Breakdown \u03b4: %.3f",
          iv_est, ifelse(is.na(breakdown), NA, breakdown)
        ),
        caption = "Shaded band = 95% CI. Estimate nullified when CI crosses zero."
      ) +
      ggplot2::theme_minimal(base_size = 12)

    out$plot <- p
  }

  invisible(out)
}
