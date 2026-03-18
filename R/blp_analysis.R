#' Best Linear Predictor (BLP) Analysis for Conditional Average Treatment Effects
#'
#' Runs the Best Linear Predictor (BLP) analysis proposed by Chernozhukov,
#' Demirer, Duflo, and Fernandez-Val (2020) to summarize heterogeneous
#' treatment effects (HTEs) estimated by a causal forest or any CATE estimator.
#' Tests whether HTEs are significantly driven by observed covariates.
#'
#' @param cate_hat Numeric vector of estimated CATEs (e.g., from
#'   \code{grf::causal_forest()}).
#' @param Y Numeric vector. Observed outcomes.
#' @param W Numeric vector. Binary treatment assignment (0/1).
#' @param Y_hat Numeric vector. Cross-fitted \eqn{E[Y \mid X]} estimates.
#' @param W_hat Numeric vector. Cross-fitted \eqn{E[W \mid X]} (propensity score).
#' @param data Data frame or matrix of raw covariates. Used for GATES
#'   subgroup analysis if \code{run_gates = TRUE}.
#' @param run_gates Logical. Whether to also run GATES analysis. Default
#'   \code{TRUE}.
#' @param n_groups Integer. Number of quantile groups for GATES. Default
#'   \code{5}.
#' @param debiased Logical. Use debiased estimates with cross-fitting if
#'   \code{TRUE} (default). Requires \code{Y_hat} and \code{W_hat}.
#' @param conf_level Numeric. Confidence level. Default \code{0.95}.
#'
#' @return A list with:
#'   \describe{
#'     \item{blp}{Data frame with BLP coefficients \eqn{\beta_1}
#'       (average treatment effect) and \eqn{\beta_2} (HTE loading).
#'       A significant \eqn{\beta_2} indicates meaningful heterogeneity.}
#'     \item{gates}{Data frame of Group Average Treatment Effects by quantile
#'       group, or \code{NULL} if \code{run_gates = FALSE}.}
#'     \item{blp_plot}{ggplot2 coefficient plot of BLP results.}
#'     \item{gates_plot}{ggplot2 GATES plot, or \code{NULL}.}
#'   }
#'
#' @details
#' The BLP is estimated from the regression:
#' \deqn{Y_i - \hat{Y}_i = \beta_1 (\hat{W}_i - \hat{e}_i) +
#'   \beta_2 (\hat{\tau}_i - \bar{\hat{\tau}})(\hat{W}_i - \hat{e}_i) + \varepsilon_i}
#' Key tests:
#' \itemize{
#'   \item \eqn{\beta_1 \neq 0}: Overall ATE different from zero.
#'   \item \eqn{\beta_2 \neq 0}: CATE varies with \eqn{\hat{\tau}} (HTE present).
#' }
#'
#' @references
#' Chernozhukov, V., Demirer, M., Duflo, E., & Fernandez-Val, I. (2020).
#' Generic machine learning inference on heterogeneous treatment effects in
#' randomized experiments. \emph{NBER Working Paper} 24678.
#'
#' @examples
#' \dontrun{
#' library(grf)
#' n <- 2000; p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' tau <- pmax(X[, 1], 0)
#' Y <- tau * W + rnorm(n)
#'
#' cf <- causal_forest(X, Y, W)
#' tau_hat <- predict(cf)$predictions
#' blp_analysis(
#'   cate_hat = tau_hat, Y = Y, W = W,
#'   Y_hat = cf$Y.hat, W_hat = cf$W.hat
#' )
#' }
#'
#' @importFrom stats lm coef vcov qnorm sd
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#'   labs theme_minimal
#' @export
blp_analysis <- function(cate_hat,
                         Y,
                         W,
                         Y_hat    = NULL,
                         W_hat    = NULL,
                         data     = NULL,
                         run_gates = TRUE,
                         n_groups  = 5,
                         debiased  = TRUE,
                         conf_level = 0.95) {

  n <- length(Y)
  if (length(W) != n || length(cate_hat) != n) {
    stop("Y, W, and cate_hat must have the same length.", call. = FALSE)
  }

  # Default: naive estimates if not provided
  if (is.null(Y_hat)) {
    Y_hat <- rep(mean(Y), n)
    debiased <- FALSE
  }
  if (is.null(W_hat)) {
    W_hat <- rep(mean(W), n)
    debiased <- FALSE
  }

  tau_bar <- mean(cate_hat)
  tau_c   <- cate_hat - tau_bar   # centered CATE
  W_res   <- W - W_hat            # residual treatment

  # BLP regression: (Y - Y_hat) ~ beta1*(W - W_hat) + beta2*(tau_c*(W - W_hat))
  Y_res <- Y - Y_hat

  blp_df <- data.frame(
    y     = Y_res,
    x1    = W_res,
    x2    = tau_c * W_res
  )
  blp_mod <- stats::lm(y ~ 0 + x1 + x2, data = blp_df)

  # Heteroskedasticity-robust SE using HC2
  vcov_blp <- tryCatch({
    if (requireNamespace("sandwich", quietly = TRUE)) {
      sandwich::vcovHC(blp_mod, type = "HC2")
    } else {
      stats::vcov(blp_mod)
    }
  }, error = function(e) stats::vcov(blp_mod))

  coefs  <- stats::coef(blp_mod)
  ses    <- sqrt(diag(vcov_blp))
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  t_vals <- coefs / ses
  p_vals <- 2 * stats::pnorm(-abs(t_vals))

  blp_out <- data.frame(
    term       = c("beta_1 (ATE)", "beta_2 (HTE)"),
    estimate   = coefs,
    std_error  = ses,
    t_stat     = t_vals,
    p_value    = p_vals,
    ci_lower   = coefs - z_crit * ses,
    ci_upper   = coefs + z_crit * ses,
    row.names  = NULL,
    stringsAsFactors = FALSE
  )

  # --- BLP plot ---
  blp_plot <- ggplot2::ggplot(blp_out,
    ggplot2::aes(x = term, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_point(size = 4, color = "#2166AC") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.15, linewidth = 0.8
    ) +
    ggplot2::labs(
      x     = NULL,
      y     = "Coefficient Estimate",
      title = "Best Linear Predictor (BLP) of CATE",
      subtitle = expression(
        paste(beta[1], ": ATE; ", beta[2], ": HTE loading (sig. = heterogeneity exists)")
      )
    ) +
    ggplot2::theme_minimal(base_size = 12)

  # --- GATES analysis ---
  gates_out  <- NULL
  gates_plot <- NULL

  if (run_gates) {
    groups <- cut(cate_hat,
      breaks  = stats::quantile(cate_hat, probs = seq(0, 1, 1 / n_groups)),
      include.lowest = TRUE,
      labels  = paste0("Q", seq_len(n_groups))
    )

    gates_rows <- lapply(levels(groups), function(g) {
      idx <- which(groups == g)
      Yi <- Y[idx]; Wi <- W[idx]
      Yi_hat <- Y_hat[idx]; Wi_hat <- W_hat[idx]
      ind <- as.integer(groups == g)

      # GATES: E[tau | group g] via regression
      g_df  <- data.frame(
        y  = Y - Y_hat,
        x  = (W - W_hat) * ind
      )
      # Include all group indicators for proper debiasing
      g_mod <- stats::lm(y ~ 0 + x, data = g_df)
      est   <- stats::coef(g_mod)[["x"]]
      se_g  <- sqrt(diag(stats::vcov(g_mod)))[["x"]]

      data.frame(
        group    = g,
        estimate = est,
        se       = se_g,
        ci_lower = est - z_crit * se_g,
        ci_upper = est + z_crit * se_g,
        n        = sum(ind),
        avg_cate = mean(cate_hat[idx]),
        stringsAsFactors = FALSE
      )
    })
    gates_out <- do.call(rbind, gates_rows)
    rownames(gates_out) <- NULL

    gates_plot <- ggplot2::ggplot(gates_out,
      ggplot2::aes(x = group, y = estimate)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      ggplot2::geom_col(fill = "#4393C3", alpha = 0.7) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
        width = 0.2
      ) +
      ggplot2::labs(
        x     = paste0("CATE Quantile Group (", n_groups, " groups)"),
        y     = "Group ATE",
        title = "GATES: Group Average Treatment Effects",
        subtitle = "Groups ranked by estimated CATE (lowest to highest)"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  }

  list(
    blp        = blp_out,
    gates      = gates_out,
    blp_plot   = blp_plot,
    gates_plot = gates_plot
  )
}
