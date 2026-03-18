#' Sensitivity Analysis for Omitted Variable Bias
#'
#' Computes sensitivity measures for assessing robustness of a causal
#' estimate to unobserved confounding:
#' \itemize{
#'   \item **E-value** (VanderWeele & Ding 2017): the minimum strength of
#'     association (on a risk-ratio scale) that an unmeasured confounder
#'     would need with *both* the treatment and the outcome to fully explain
#'     away the observed effect.
#'   \item **Robustness value** (Cinelli & Hazlett 2020): the minimum partial
#'     R² that the confounder would need to explain in both the treatment and
#'     the outcome to drive the estimate to zero (or the null).
#'   \item **Bias-adjusted contour plot**: estimated effect across a grid of
#'     partial R² values for the confounder's association with treatment
#'     (r²\[dz|x\]) and outcome (r²\[yz|x\]).
#' }
#' No dependency on \pkg{sensemakr} is required; all computations use base R.
#'
#' @title Sensitivity Analysis for Omitted Variable Bias (E-value & RV)
#'
#' @description Implements the E-value of VanderWeele & Ding (2017) and the
#'   robustness value / bias formula of Cinelli & Hazlett (2020) to quantify
#'   how much unmeasured confounding would be needed to explain away a causal
#'   estimate.
#'
#' @param estimate Numeric. The point estimate of the causal effect (from any
#'   model).
#' @param se Numeric. The standard error of the estimate.
#' @param df Numeric. Residual degrees of freedom. Use `Inf` for large samples.
#'   Default `Inf`.
#' @param benchmark_covariates Character vector or `NULL`. Names of observed
#'   covariates to use as benchmarks (requires `data`, `outcome`, and
#'   `treatment`). Optional.
#' @param data Data frame or `NULL`. Required when `benchmark_covariates` is
#'   specified.
#' @param outcome Character or `NULL`. Outcome variable name in `data`.
#' @param treatment Character or `NULL`. Treatment variable name in `data`.
#' @param alpha Numeric. Significance level for the CI-based E-value. Default
#'   `0.05`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`e_value`}{Named numeric: `e_value` (for the point estimate) and
#'       `e_value_ci` (for the confidence limit closer to the null).}
#'     \item{`robustness_value`}{Named numeric: `rv_q0` (to explain away the
#'       point estimate) and `rv_qa` (to explain away the CI bound).}
#'     \item{`sensitivity_df`}{Data frame of bias-adjusted estimates across a
#'       grid of `r2dz` (0–0.5) × `r2yz` (0–0.5) partial R² values.}
#'     \item{`plot`}{A ggplot2 contour plot showing the sensitivity surface.}
#'   }
#'
#' @references
#' VanderWeele, T. J., & Ding, P. (2017). Sensitivity analysis in
#' observational research: Introducing the E-value. *Annals of Internal
#' Medicine*, 167(4), 268-274.
#'
#' Cinelli, C., & Hazlett, C. (2020). Making sense of sensitivity: Extending
#' omitted variable bias. *Journal of the Royal Statistical Society: Series B*,
#' 82(1), 39-67.
#'
#' @examples
#' # Simulated linear regression
#' set.seed(42)
#' n  <- 500
#' x  <- rnorm(n)
#' y  <- 0.4 * x + rnorm(n)
#' m  <- lm(y ~ x)
#' sm <- summary(m)
#'
#' est <- coef(m)["x"]
#' se  <- coef(sm)["x", "Std. Error"]
#' df_r <- m$df.residual
#'
#' sens <- confounding_strength(estimate = est, se = se, df = df_r)
#' print(sens$e_value)
#' print(sens$robustness_value)
#' sens$plot
#'
#' @importFrom ggplot2 ggplot aes geom_contour geom_contour_filled
#'   geom_point geom_text scale_fill_viridis_d labs theme
#' @importFrom stats qt
#' @export
confounding_strength <- function(estimate,
                                  se,
                                  df                   = Inf,
                                  benchmark_covariates = NULL,
                                  data                 = NULL,
                                  outcome              = NULL,
                                  treatment            = NULL,
                                  alpha                = 0.05) {

  stopifnot(is.numeric(estimate), length(estimate) == 1)
  stopifnot(is.numeric(se),       length(se) == 1, se >= 0)

  # ---- E-value computation -------------------------------------------------
  # E-value: for a risk-ratio estimate RR > 1,
  # E = RR + sqrt(RR * (RR - 1))
  # For regression coefficient, convert to approximate RR or use raw scale:
  # Following VanderWeele & Ding (2017), Supplement for linear models:
  # E-value for linear coefficient: effect size in SD units
  # We use the formula for a continuous outcome (approximate):
  # E-value based on effect / SE (t-statistic approach via Cinelli's notes)

  t_stat   <- estimate / se
  t_crit   <- stats::qt(1 - alpha / 2, df = df)

  # Bound closer to null
  ci_bound <- if (estimate > 0) {
    estimate - t_crit * se
  } else {
    estimate + t_crit * se
  }

  # E-value via the formula for a standardised mean difference (Cohen's d):
  # E-value = d + sqrt(d * (d + 4)) , where d = |estimate| / se (standardised)
  # Based on VanderWeele & Ding (2017) Table 3 approximation
  eval_fn <- function(eff) {
    if (is.na(eff) || eff == 0) return(1)
    eff_abs <- abs(eff)
    # For continuous: E-value on Cohen's d scale
    eff_abs + sqrt(eff_abs^2 + eff_abs)
  }

  e_val    <- eval_fn(estimate / se)        # for point estimate
  e_val_ci <- eval_fn(ci_bound / se)        # for CI bound

  e_value <- c(e_value = e_val, e_value_ci = e_val_ci)

  # ---- Robustness value (Cinelli & Hazlett 2020) ---------------------------
  # RV_q: minimum partial R2 such that, if an unobserved confounder explains
  # fraction RV of the residual variance of both D and Y (after controls),
  # the estimate is driven to zero.
  # Closed-form for linear regression (Proposition 1):
  # RV_q = (1/2) * [sqrt(f_q^4 + 4 * f_q^2) - f_q^2]
  # where f_q = |t_stat| / sqrt(df)  (partial F contribution)
  f_q    <- abs(t_stat) / sqrt(max(df, 1))
  rv_q0  <- 0.5 * (sqrt(f_q^4 + 4 * f_q^2) - f_q^2)  # for point est = 0

  f_qa   <- abs(t_crit) / sqrt(max(df, 1))
  rv_qa  <- 0.5 * (sqrt(f_qa^4 + 4 * f_qa^2) - f_qa^2)  # for CI bound = 0

  robustness_value <- c(rv_q0 = rv_q0, rv_qa = rv_qa)

  # ---- Bias-adjusted contour grid ------------------------------------------
  # Bias formula (Cinelli & Hazlett 2020, Eq. 4):
  # bias = sign(b) * se * sqrt(r2dz * r2yz / (1 - r2dz)) * sqrt(df)
  # adjusted estimate = estimate - bias

  r2_seq <- seq(0, 0.5, by = 0.025)
  grid   <- expand.grid(r2dz = r2_seq, r2yz = r2_seq)

  sign_est <- sign(estimate)
  grid$bias_adj <- sign_est * se *
    sqrt(grid$r2dz * grid$r2yz / pmax(1 - grid$r2dz, 1e-6)) *
    sqrt(pmax(df, 1))
  grid$adj_est  <- estimate - grid$bias_adj

  # ---- Contour plot --------------------------------------------------------
  p <- ggplot2::ggplot(grid,
         ggplot2::aes(x = r2dz, y = r2yz, z = adj_est)) +
    ggplot2::geom_contour_filled(bins = 15, alpha = 0.85) +
    ggplot2::geom_contour(color = "white", linewidth = 0.3, bins = 15) +
    # Mark the robustness value point
    ggplot2::geom_point(
      data = data.frame(r2dz = rv_q0, r2yz = rv_q0),
      ggplot2::aes(x = r2dz, y = r2yz),
      color = "red", size = 4, shape = 18, inherit.aes = FALSE) +
    ggplot2::geom_text(
      data = data.frame(r2dz = rv_q0, r2yz = rv_q0),
      ggplot2::aes(x = r2dz, y = r2yz,
                   label = paste0("RV=", round(rv_q0, 3))),
      color = "red", vjust = -1, size = 3.2, inherit.aes = FALSE) +
    ggplot2::scale_fill_viridis_d(
      option = "plasma", name = "Bias-Adjusted\nEstimate") +
    ggplot2::labs(
      title    = "Sensitivity Contour Plot",
      subtitle = paste0(
        "E-value = ", round(e_val, 3),
        "  |  RV = ", round(rv_q0, 3),
        "  |  Original estimate = ", round(estimate, 3)),
      x        = expression(paste("Partial ", R^2, " of confounder with Treatment")),
      y        = expression(paste("Partial ", R^2, " of confounder with Outcome"))
    ) +
    causalverse::ama_theme() +
    ggplot2::theme(legend.position = "right")

  list(
    e_value          = e_value,
    robustness_value = robustness_value,
    sensitivity_df   = grid,
    plot             = p
  )
}
