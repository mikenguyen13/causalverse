#' Minimum Detectable Effect (MDE) Calculator
#'
#' Computes the Minimum Detectable Effect (MDE) for a range of experimental
#' designs including simple two-sample t-tests, DiD, RD, and clustered
#' randomized trials. Also produces power curves for visualization.
#'
#' @param n Numeric. Total sample size (or sample per arm if
#'   \code{per_arm = TRUE}).
#' @param per_arm Logical. If \code{TRUE}, \code{n} is interpreted as sample
#'   per arm. Default \code{FALSE}.
#' @param design Character. Experimental design:
#'   \code{"ttest"} (default), \code{"did"}, \code{"rd"}, \code{"cluster"}.
#' @param sd Numeric. Standard deviation of the outcome. Default \code{1}.
#' @param alpha Numeric. Type I error rate. Default \code{0.05}.
#' @param power Numeric. Desired power. Default \code{0.80}.
#' @param two_sided Logical. Whether the test is two-sided. Default
#'   \code{TRUE}.
#' @param icc Numeric. Intraclass correlation for cluster designs. Default
#'   \code{0}.
#' @param cluster_size Integer. Average cluster size. Default \code{1}.
#' @param r2 Numeric. R-squared of covariates (variance reduction from
#'   regression adjustment). Default \code{0} (no adjustment).
#' @param kappa Numeric. Ratio of control to treated observations
#'   (1 = equal sizes, 2 = twice as many controls). Default \code{1}.
#' @param plot Logical. Whether to produce a power curve. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{mde}{Numeric. Minimum detectable effect (in outcome units).}
#'     \item{mde_std}{Numeric. Standardized MDE (Cohen's d).}
#'     \item{design_effect}{Numeric. Design effect from clustering.}
#'     \item{effective_n}{Numeric. Effective sample size after design effect.}
#'     \item{plot}{ggplot2 power curve, or \code{NULL} if \code{plot = FALSE}.}
#'   }
#'
#' @details
#' The MDE is the smallest true effect that would be detected with probability
#' \code{power} at significance level \code{alpha}:
#' \deqn{MDE = (t_{1-\alpha/2} + t_{1-\beta}) \cdot SE(\hat{\tau})}
#'
#' For clustered designs, the design effect is:
#' \deqn{DEFF = 1 + (\bar{m} - 1)\rho}
#' where \eqn{\bar{m}} is the average cluster size and \eqn{\rho} is the ICC.
#'
#' @examples
#' # Simple t-test
#' mde_calc(n = 200)
#'
#' # DiD with clustering
#' mde_calc(n = 500, design = "did", icc = 0.05, cluster_size = 25)
#'
#' # With covariate adjustment (reduces variance)
#' mde_calc(n = 200, r2 = 0.3)
#'
#' @importFrom stats qnorm pnorm
#' @importFrom ggplot2 ggplot aes geom_line geom_vline geom_hline
#'   labs scale_y_continuous theme_minimal annotate
#' @export
mde_calc <- function(n,
                     per_arm      = FALSE,
                     design       = c("ttest", "did", "rd", "cluster"),
                     sd           = 1,
                     alpha        = 0.05,
                     power        = 0.80,
                     two_sided    = TRUE,
                     icc          = 0,
                     cluster_size = 1,
                     r2           = 0,
                     kappa        = 1,
                     plot         = TRUE) {

  design <- match.arg(design)

  z_alpha <- if (two_sided) stats::qnorm(1 - alpha / 2) else stats::qnorm(1 - alpha)
  z_power <- stats::qnorm(power)

  # Total and per-arm n
  if (per_arm) {
    n_treat <- n
    n_ctrl  <- round(kappa * n)
    n_total <- n_treat + n_ctrl
  } else {
    n_total <- n
    n_treat <- round(n_total / (1 + kappa))
    n_ctrl  <- n_total - n_treat
  }

  # Design effect
  deff <- if (icc > 0 && cluster_size > 1) {
    1 + (cluster_size - 1) * icc
  } else {
    1
  }

  # Variance reduction from covariate adjustment
  var_factor <- (1 - r2) * sd^2

  # Design-specific SE multiplier
  se_base <- switch(design,
    "ttest"   = sqrt(var_factor * deff * (1 / n_treat + 1 / n_ctrl)),
    "did"     = sqrt(var_factor * deff * 2 * (1 / n_treat + 1 / n_ctrl)),
    "rd"      = sqrt(var_factor * deff * 4 * (1 / n_treat + 1 / n_ctrl)),  # ~4x inflation for RD
    "cluster" = sqrt(var_factor * deff * (1 / n_treat + 1 / n_ctrl))
  )

  mde     <- (z_alpha + z_power) * se_base
  mde_std <- mde / sd

  eff_n <- n_total / deff

  out <- list(
    mde           = round(mde, 6),
    mde_std       = round(mde_std, 6),
    se            = round(se_base, 6),
    design_effect = round(deff, 4),
    effective_n   = round(eff_n, 1),
    n_total       = n_total,
    n_treat       = n_treat,
    n_ctrl        = n_ctrl,
    design        = design,
    plot          = NULL
  )

  if (plot) {
    ns <- seq(10, max(n_total * 3, 200), length.out = 200)

    power_fn <- function(nt) {
      nt_arm  <- round(nt / (1 + kappa))
      nc_arm  <- nt - nt_arm
      se_n <- switch(design,
        "ttest"   = sqrt(var_factor * deff * (1 / nt_arm + 1 / nc_arm)),
        "did"     = sqrt(var_factor * deff * 2 * (1 / nt_arm + 1 / nc_arm)),
        "rd"      = sqrt(var_factor * deff * 4 * (1 / nt_arm + 1 / nc_arm)),
        "cluster" = sqrt(var_factor * deff * (1 / nt_arm + 1 / nc_arm))
      )
      (z_alpha + z_power) * se_n
    }

    mdes <- vapply(ns, power_fn, numeric(1))
    df   <- data.frame(n = ns, mde = mdes)

    p <- ggplot2::ggplot(df, ggplot2::aes(x = n, y = mde)) +
      ggplot2::geom_line(color = "#2166AC", linewidth = 1) +
      ggplot2::geom_vline(xintercept = n_total, linetype = "dashed",
                          color = "red", alpha = 0.7) +
      ggplot2::geom_hline(yintercept = mde, linetype = "dotted",
                          color = "gray40") +
      ggplot2::annotate("point", x = n_total, y = mde,
                        size = 4, color = "#D73027") +
      ggplot2::annotate("text",
        x = n_total * 1.05, y = mde,
        label = paste0("MDE = ", round(mde, 3)),
        hjust = 0, size = 3.5
      ) +
      ggplot2::labs(
        x     = "Total Sample Size",
        y     = "Minimum Detectable Effect",
        title = paste0("MDE Curve (", toupper(design), " design)"),
        subtitle = sprintf(
          "\u03b1=%.2f, power=%.2f, DEFF=%.2f, R\u00b2=%.2f",
          alpha, power, deff, r2
        )
      ) +
      ggplot2::scale_y_continuous(limits = c(0, NA)) +
      ggplot2::theme_minimal(base_size = 12)

    out$plot <- p
  }

  invisible(out)
}
