#' Qini Curve and AUUC for Uplift / Policy Evaluation
#'
#' Computes and plots the Qini curve and Area Under the Uplift Curve (AUUC),
#' which measure the quality of treatment effect heterogeneity estimates for
#' targeting / policy evaluation. Compares a CATE-based targeting rule against
#' random assignment and perfect targeting benchmarks.
#'
#' @param cate_hat Numeric vector of estimated CATEs.
#' @param Y Numeric vector. Observed outcomes.
#' @param W Numeric vector. Binary treatment assignment (0 or 1).
#' @param compare_random Logical. If \code{TRUE} (default), plot the random
#'   targeting baseline.
#' @param compare_oracle Logical. If \code{FALSE} (default), do not plot an
#'   oracle curve (true CATEs rarely available).
#' @param n_bins Integer. Number of bins for the curve. Default \code{100}.
#' @param conf_level Numeric. Confidence level for bootstrap CI on AUUC.
#'   Default \code{0.95}.
#' @param boot_reps Integer. Bootstrap replicates for AUUC CI. Default
#'   \code{500}.
#' @param seed Integer. Random seed. Default \code{42}.
#'
#' @return A list with:
#'   \describe{
#'     \item{auuc}{Numeric. Area under the Qini curve (normalized 0-1).}
#'     \item{auuc_ci}{Numeric vector of length 2. Bootstrap confidence
#'       interval for AUUC.}
#'     \item{qini_df}{Data frame with columns \code{fraction}, \code{uplift}.}
#'     \item{plot}{ggplot2 Qini curve plot.}
#'   }
#'
#' @details
#' The Qini curve plots the fraction of treated units (ranked by
#' estimated CATE, highest first) against the cumulative incremental
#' outcome (uplift). The AUUC normalizes this to a value between 0 and 1, relative to
#' the perfect oracle. Values above 0.5 indicate better-than-random targeting.
#'
#' @references
#' Radcliffe, N. J. (2007). Using control groups to target on predicted lift:
#' Building and assessing uplift models. \emph{Direct Marketing Analytics
#' Journal}, 1(3), 14-21.
#'
#' Athey, S., & Imbens, G. W. (2017). The econometrics of randomized
#' experiments. In \emph{Handbook of Economic Field Experiments} (Vol. 1,
#' pp. 73-140). North-Holland.
#'
#' @examples
#' \dontrun{
#' library(grf)
#' n <- 1000; p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' tau <- pmax(X[, 1], 0)
#' Y   <- tau * W + rnorm(n)
#' cf  <- causal_forest(X, Y, W)
#' tau_hat <- predict(cf)$predictions
#'
#' result <- qini_curve(tau_hat, Y, W)
#' result$plot
#' result$auuc
#' }
#'
#' @importFrom stats quantile sd
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs
#'   scale_x_continuous theme_minimal
#' @export
qini_curve <- function(cate_hat,
                       Y,
                       W,
                       compare_random = TRUE,
                       compare_oracle = FALSE,
                       n_bins         = 100,
                       conf_level     = 0.95,
                       boot_reps      = 500,
                       seed           = 42) {

  n <- length(Y)
  if (length(W) != n || length(cate_hat) != n) {
    stop("Y, W, and cate_hat must have the same length.", call. = FALSE)
  }

  W <- as.integer(W)
  p_treat <- mean(W)

  # --- Compute Qini curve ---
  compute_qini <- function(tau_hat, Y, W, n_bins) {
    # Rank by estimated CATE descending
    ord   <- order(tau_hat, decreasing = TRUE)
    fracs <- seq(0, 1, length.out = n_bins + 1)
    uplifts <- numeric(n_bins + 1)

    for (k in seq_len(n_bins + 1)) {
      if (k == 1) { uplifts[k] <- 0; next }
      frac  <- fracs[k]
      n_top <- max(1, round(frac * n))
      top   <- ord[seq_len(n_top)]
      rest  <- ord[seq(n_top + 1, n)]

      mean_Y1_top <- if (sum(W[top] == 1) > 0) mean(Y[top][W[top] == 1]) else 0
      mean_Y0_top <- if (sum(W[top] == 0) > 0) mean(Y[top][W[top] == 0]) else 0
      tau_top <- mean_Y1_top - mean_Y0_top

      # Qini: incremental benefit = tau_top * fraction_treated_in_top
      uplifts[k] <- tau_top * frac
    }
    data.frame(fraction = fracs, uplift = uplifts)
  }

  qini_df   <- compute_qini(cate_hat, Y, W, n_bins)
  auuc_raw  <- mean(qini_df$uplift)

  # Random baseline AUUC (treat uniformly)
  overall_uplift <- mean(Y[W == 1]) - mean(Y[W == 0])
  random_df <- data.frame(
    fraction = seq(0, 1, length.out = n_bins + 1),
    uplift   = seq(0, 1, length.out = n_bins + 1) * overall_uplift
  )
  auuc_random <- mean(random_df$uplift)

  # Normalized AUUC ∈ [0, 1]: 0 = random, 1 = perfect
  auuc_norm <- if (auuc_random != 0) (auuc_raw - auuc_random) / abs(auuc_random) else auuc_raw

  # --- Bootstrap CI ---
  set.seed(seed)
  boot_auucs <- numeric(boot_reps)
  for (b in seq_len(boot_reps)) {
    idx <- sample(n, replace = TRUE)
    q_b <- compute_qini(cate_hat[idx], Y[idx], W[idx], n_bins)
    boot_auucs[b] <- mean(q_b$uplift)
  }
  alpha  <- 1 - conf_level
  auuc_ci <- stats::quantile(boot_auucs, c(alpha / 2, 1 - alpha / 2))

  # --- Plot ---
  p <- ggplot2::ggplot(qini_df, ggplot2::aes(x = fraction, y = uplift)) +
    ggplot2::geom_line(color = "#2166AC", linewidth = 1.2,
                       ggplot2::aes(linetype = "CATE-Based Targeting")) +
    ggplot2::labs(
      x     = "Fraction of Population Treated (Ranked by Estimated CATE)",
      y     = "Cumulative Uplift",
      title = "Qini Curve",
      subtitle = sprintf(
        "AUUC = %.4f (95%% CI: [%.4f, %.4f])",
        auuc_raw, auuc_ci[1], auuc_ci[2]
      ),
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (compare_random) {
    p <- p +
      ggplot2::geom_line(
        data = random_df,
        ggplot2::aes(x = fraction, y = uplift, linetype = "Random Targeting"),
        color = "gray50", linewidth = 0.8, linetype = "dashed"
      )
  }

  list(
    auuc    = auuc_raw,
    auuc_ci = auuc_ci,
    qini_df = qini_df,
    plot    = p
  )
}
