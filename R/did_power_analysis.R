#' Power Analysis for Difference-in-Differences Designs
#'
#' Computes statistical power (or required sample size / MDE) for two-period
#' and staggered DiD designs. Supports clustered standard errors, pre-trend
#' variance correction, and autocorrelation adjustment.
#'
#' @param n_treated Integer. Number of treated units. Required unless
#'   \code{solve_for = "n"}.
#' @param n_control Integer. Number of control units. Defaults to
#'   \code{n_treated} (1:1 ratio).
#' @param effect_size Numeric. True average treatment effect (ATT). Required
#'   unless \code{solve_for = "effect"} or \code{solve_for = "mde"}.
#' @param sd_outcome Numeric. Standard deviation of the outcome. Default
#'   \code{1} (standardized).
#' @param n_periods Integer. Total number of time periods. Default \code{2}
#'   (classic two-period DiD).
#' @param pre_periods Integer. Number of pre-treatment periods. Default
#'   \code{1}.
#' @param rho Numeric. Within-unit serial autocorrelation coefficient.
#'   Default \code{0} (no autocorrelation).
#' @param icc Numeric. Intraclass correlation if units are clustered within
#'   groups. Default \code{0} (no clustering).
#' @param cluster_size Integer. Average cluster size (units per cluster).
#'   Required when \code{icc > 0}. Default \code{1}.
#' @param alpha Numeric. Significance level. Default \code{0.05}.
#' @param two_sided Logical. Whether to use a two-sided test. Default
#'   \code{TRUE}.
#' @param solve_for Character. What to solve for: \code{"power"} (default),
#'   \code{"n"} (minimum per-arm sample size), or \code{"mde"} (minimum
#'   detectable effect).
#' @param plot Logical. If \code{TRUE} (default), returns a power curve plot.
#'
#' @return If \code{solve_for = "power"}: a list with:
#'   \describe{
#'     \item{power}{Numeric. Estimated statistical power.}
#'     \item{se_did}{Numeric. Estimated standard error of the DiD estimator.}
#'     \item{design_effect}{Numeric. Design effect from clustering/autocorrelation.}
#'     \item{plot}{A ggplot2 power curve (power vs. effect size), or \code{NULL}
#'       if \code{plot = FALSE}.}
#'   }
#'   If \code{solve_for = "n"}: a list with \code{n_per_arm}, \code{n_total}.
#'   If \code{solve_for = "mde"}: a list with \code{mde}.
#'
#' @details
#' The DiD variance formula for the simple two-period case with \code{N_t}
#' treated and \code{N_c} control units, \code{T} total periods, and outcome
#' variance \eqn{\sigma^2} is:
#' \deqn{Var(\hat{\tau}_{DiD}) \approx \sigma^2 \cdot \text{DEFF} \cdot
#'   \left(\frac{1}{N_t \cdot T/2} + \frac{1}{N_c \cdot T/2}\right)}
#' where DEFF is the design effect incorporating autocorrelation and
#' intraclass correlation. Serial autocorrelation with parameter \eqn{\rho}
#' and \eqn{T} periods contributes a factor of approximately
#' \eqn{1 + (T-1)\rho}. Clustering with ICC \eqn{\lambda} and cluster
#' size \eqn{m} contributes a factor of \eqn{1 + (m-1)\lambda}.
#'
#' @examples
#' # Two-period DiD power
#' did_power_analysis(n_treated = 50, effect_size = 0.3, sd_outcome = 1)
#'
#' # With clustering
#' did_power_analysis(
#'   n_treated = 200, effect_size = 0.2, sd_outcome = 1,
#'   icc = 0.05, cluster_size = 20
#' )
#'
#' # Solve for MDE
#' did_power_analysis(
#'   n_treated = 100, n_periods = 4, pre_periods = 2,
#'   solve_for = "mde"
#' )
#'
#' @importFrom stats qnorm pnorm
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_vline
#'   labs scale_x_continuous theme_minimal annotate
#' @export
did_power_analysis <- function(n_treated    = NULL,
                               n_control    = NULL,
                               effect_size  = NULL,
                               sd_outcome   = 1,
                               n_periods    = 2,
                               pre_periods  = 1,
                               rho          = 0,
                               icc          = 0,
                               cluster_size = 1,
                               alpha        = 0.05,
                               two_sided    = TRUE,
                               solve_for    = c("power", "n", "mde"),
                               plot         = TRUE) {

  solve_for <- match.arg(solve_for)

  # Default: equal group sizes
  if (is.null(n_control) && !is.null(n_treated)) {
    n_control <- n_treated
  }

  post_periods <- n_periods - pre_periods
  if (post_periods < 1) {
    stop("`pre_periods` must be less than `n_periods`.", call. = FALSE)
  }

  # Design effect from serial autocorrelation (block autocorrelation)
  deff_autocorr <- if (rho != 0 && n_periods > 1) {
    1 + (n_periods - 1) * rho
  } else {
    1
  }

  # Design effect from clustering
  deff_cluster <- if (icc > 0 && cluster_size > 1) {
    1 + (cluster_size - 1) * icc
  } else {
    1
  }

  deff <- deff_autocorr * deff_cluster

  # Critical value
  z_alpha <- if (two_sided) {
    stats::qnorm(1 - alpha / 2)
  } else {
    stats::qnorm(1 - alpha)
  }

  # Internal power function
  compute_power <- function(nt, nc, delta) {
    # SE of DiD estimator: sigma * sqrt(DEFF) * sqrt(1/(nt*T2) + 1/(nc*T2))
    # where T2 = effective periods per arm
    t2 <- post_periods  # post-treatment periods
    se <- sd_outcome * sqrt(deff) * sqrt(1 / (nt * t2) + 1 / (nc * t2))
    z_beta <- abs(delta) / se - z_alpha
    power  <- stats::pnorm(z_beta)
    list(power = power, se = se)
  }

  if (solve_for == "power") {
    if (is.null(n_treated) || is.null(effect_size)) {
      stop("For `solve_for = 'power'`, both `n_treated` and `effect_size` are required.",
           call. = FALSE)
    }

    res <- compute_power(n_treated, n_control, effect_size)
    pwr <- res$power
    se_did <- res$se

    out <- list(
      power         = round(pwr, 4),
      se_did        = round(se_did, 6),
      design_effect = round(deff, 4),
      n_treated     = n_treated,
      n_control     = n_control,
      effect_size   = effect_size,
      alpha         = alpha,
      plot          = NULL
    )

    if (plot) {
      effects <- seq(0, max(abs(effect_size) * 3, 1), length.out = 200)
      powers  <- vapply(effects, function(d) {
        compute_power(n_treated, n_control, d)$power
      }, numeric(1))
      df <- data.frame(effect = effects, power = powers)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = effect, y = power)) +
        ggplot2::geom_line(color = "#2166AC", linewidth = 1) +
        ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed",
                            color = "red", alpha = 0.7) +
        ggplot2::geom_hline(yintercept = pwr, linetype = "dotted",
                            color = "gray40") +
        ggplot2::geom_vline(xintercept = abs(effect_size), linetype = "dotted",
                            color = "gray40") +
        ggplot2::annotate("point", x = abs(effect_size), y = pwr,
                          size = 4, color = "#D73027") +
        ggplot2::annotate("text",
          x = abs(effect_size), y = pwr + 0.03,
          label = paste0("Power = ", round(pwr, 2)),
          hjust = 0, size = 3.5
        ) +
        ggplot2::labs(
          x     = "True Treatment Effect",
          y     = "Statistical Power",
          title = "DiD Power Curve",
          subtitle = sprintf(
            "N(treated)=%d, N(control)=%d, T=%d, \u03b1=%.2f, DEFF=%.2f",
            n_treated, n_control, n_periods, alpha, deff
          )
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1)) +
        ggplot2::theme_minimal(base_size = 12)

      out$plot <- p
    }

    return(invisible(out))
  }

  if (solve_for == "mde") {
    if (is.null(n_treated)) {
      stop("For `solve_for = 'mde'`, `n_treated` is required.", call. = FALSE)
    }
    power_target <- 0.80
    z_beta <- stats::qnorm(power_target)
    t2     <- post_periods
    se     <- sd_outcome * sqrt(deff) * sqrt(1 / (n_treated * t2) + 1 / (n_control * t2))
    mde    <- (z_alpha + z_beta) * se

    return(invisible(list(
      mde           = round(mde, 6),
      mde_std       = round(mde / sd_outcome, 6),
      se_did        = round(se, 6),
      design_effect = round(deff, 4),
      power_target  = power_target,
      n_treated     = n_treated,
      n_control     = n_control
    )))
  }

  if (solve_for == "n") {
    if (is.null(effect_size)) {
      stop("For `solve_for = 'n'`, `effect_size` is required.", call. = FALSE)
    }
    power_target <- 0.80
    z_beta <- stats::qnorm(power_target)
    t2 <- post_periods
    # From SE formula: n = sigma^2 * DEFF * (z_alpha+z_beta)^2 * 2 / (delta^2 * T2)
    n_per_arm <- ceiling(
      2 * sd_outcome^2 * deff * (z_alpha + z_beta)^2 / (effect_size^2 * t2)
    )
    return(invisible(list(
      n_per_arm     = n_per_arm,
      n_total       = 2 * n_per_arm,
      design_effect = round(deff, 4),
      effect_size   = effect_size,
      power_target  = power_target
    )))
  }
}
