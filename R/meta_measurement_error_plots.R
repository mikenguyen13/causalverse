#' Plots for the Measurement-Error Meta-Regression
#'
#' ggplot2 versions of the figures in Pancost and Schaller (2026):
#' \describe{
#'   \item{\code{meta_me_plot_scatter()}}{OLS against IV coefficients with
#'     the fitted meta-OLS line and the noise-only benchmark (Figure 4).}
#'   \item{\code{meta_me_plot_corr_bias()}}{Implied \eqn{\theta} against the
#'     correlation between effects and OVB (Proposition 4, Figure 5).}
#'   \item{\code{meta_me_plot_tdist()}}{Observed and simulated |t| with the
#'     unpublished mass implied by the publication-bias model (Figure 7).}
#'   \item{\code{meta_me_plot_heterogeneity()}}{Implied \eqn{\theta} under
#'     heterogeneous treatment effects (Figure 8).}
#'   \item{\code{meta_me_plot_compare()}}{IV-OLS magnitude comparisons by
#'     expected bias direction (Figures 3 and 6).}
#' }
#'
#' @param fit A \code{meta_me_ols} object (\code{within = "none"}).
#' @return A ggplot object.
#' @name meta_me_plots
#' @family meta_me
NULL

#' @rdname meta_me_plots
#' @param limits Optional axis limits \code{list(x = c(..), y = c(..))}.
#' @export
meta_me_plot_scatter <- function(fit, limits = NULL) {
  d <- fit$data
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$b_iv, y = .data$b_ols)) +
    ggplot2::geom_point(shape = 1, alpha = 0.6) +
    ggplot2::geom_abline(intercept = fit$intercept, slope = fit$slope, linewidth = 0.9) +
    ggplot2::geom_abline(intercept = 0, slope = fit$rho2, colour = "grey60", linewidth = 0.9) +
    ggplot2::labs(x = "IV coefficient", y = "OLS coefficient",
                  caption = sprintf("Black: meta-OLS fit (slope %.3f). Grey: slope rho^2 = %.3f (no measurement error). Implied theta = %.3f.",
                                    fit$slope, fit$rho2, fit$theta)) +
    ggplot2::theme_bw()
  if (!is.null(limits)) p <- p + ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y)
  p
}

#' @rdname meta_me_plots
#' @param var_theta Values of \eqn{var(\theta_j)} (one line each).
#' @export
meta_me_plot_corr_bias <- function(fit, var_theta = c(0, 0.03, 0.059)) {
  R <- seq(-0.99, 0.99, length.out = 397)
  g <- meta_me_corr_bias(fit, R = R, var_theta = var_theta)
  g$var_theta <- factor(sprintf("var(theta_j) = %.3g", g$var_theta))
  ggplot2::ggplot(g, ggplot2::aes(x = .data$R, y = .data$theta, linetype = .data$var_theta)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "corr(beta_j, gamma_j)", y = "Implied theta", linetype = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
}

#' @rdname meta_me_plots
#' @param pb A \code{meta_me_pubbias} object.
#' @param data Prepared data (for the observed panel).
#' @param max_t Truncation for display. Default 6.
#' @export
meta_me_plot_tdist <- function(pb, data, max_t = 6) {
  obs <- data.frame(t = abs(data$b_iv / data$se_iv), panel = "Data", status = "Published")
  t_all <- abs(pb$sims$all_b_iv) / pb$sims$all_z_iv
  sim <- data.frame(t = t_all, panel = "Model",
                    status = ifelse(pb$sims$keep, "Published", "Unpublished"))
  d <- rbind(obs, sim)
  d <- d[d$t <= max_t, ]
  ggplot2::ggplot(d, ggplot2::aes(x = .data$t, fill = .data$status)) +
    ggplot2::geom_histogram(breaks = seq(0, max_t, by = 0.5), colour = "black", position = "stack") +
    ggplot2::geom_vline(xintercept = 1.96, linetype = 2) +
    ggplot2::facet_wrap(~ panel, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c(Published = "grey55", Unpublished = "white")) +
    ggplot2::labs(x = "|t| of IV estimate", y = "Count", fill = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
}

#' @rdname meta_me_plots
#' @param ... Passed to \code{meta_me_heterogeneity()}.
#' @export
meta_me_plot_heterogeneity <- function(fit, var_theta = c(1e-8, 0.04, 0.059), ...) {
  g <- meta_me_heterogeneity(fit, R = seq(-0.99, 0.99, length.out = 397),
                             var_theta = var_theta, ...)
  g$var_theta <- factor(sprintf("var(theta_j) = %.3g", g$var_theta))
  ggplot2::ggplot(g, ggplot2::aes(x = .data$R, y = .data$theta, linetype = .data$var_theta)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "corr(LATE, H_j)", y = "Implied theta", linetype = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
}

#' @rdname meta_me_plots
#' @param compare Output of \code{meta_me_compare()}.
#' @export
meta_me_plot_compare <- function(compare) {
  cmp <- compare[compare$group != "All", ]
  long <- rbind(
    data.frame(group = cmp$group, panel = "Share of pairs (%)",
               stat = "|IV| > |OLS| (same sign)", value = cmp$pct_iv_bigger_same_sign),
    data.frame(group = cmp$group, panel = "Share of pairs (%)",
               stat = "Switch sign", value = cmp$pct_switch_sign),
    data.frame(group = cmp$group, panel = "|IV / OLS|", stat = "Mean", value = cmp$mean_ratio),
    data.frame(group = cmp$group, panel = "|IV / OLS|", stat = "Median", value = cmp$median_ratio))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$group, y = .data$value, fill = .data$stat)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.75, colour = "black") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", .data$value)),
                       position = ggplot2::position_dodge(0.8), vjust = -0.3, size = 3) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 1) +
    ggplot2::scale_fill_grey(start = 0.95, end = 0.45) +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme_bw() + ggplot2::theme(legend.position = "bottom")
}
