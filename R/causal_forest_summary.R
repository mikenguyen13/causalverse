#' Comprehensive Summary of Causal Forest Results
#'
#' Produces a complete, publication-ready summary of a \pkg{grf} causal
#' forest object, including: ATE with inference, heterogeneity assessment
#' (BLP, GATES), CATE distribution, most important features, and
#' policy-relevant summaries.
#'
#' @param cf A \code{causal_forest} object from \pkg{grf}.
#' @param X Data frame or matrix of covariates (same as used for fitting).
#' @param feature_names Character vector. Names for the feature columns.
#'   If \code{NULL}, uses column names of \code{X}.
#' @param n_groups Integer. Number of groups for GATES analysis. Default
#'   \code{5}.
#' @param top_features Integer. Number of top features to show in
#'   importance plot. Default \code{10}.
#' @param plot Logical. Produce plots. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{ate}{Data frame: ATE estimate with SE, t-stat, p-value, CI.}
#'     \item{cate_summary}{Summary statistics of CATE distribution.}
#'     \item{blp}{BLP analysis results (from \code{blp_analysis()}).}
#'     \item{feature_importance}{Data frame of variable importance scores.}
#'     \item{plot_cate_dist}{ggplot2: CATE distribution histogram.}
#'     \item{plot_importance}{ggplot2: feature importance bar chart.}
#'     \item{plot_blp}{ggplot2: BLP coefficient plot.}
#'     \item{plot_gates}{ggplot2: GATES plot.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(grf)
#' n <- 1000; p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("X", 1:p)
#' W <- rbinom(n, 1, 0.5)
#' tau <- pmax(X[, 1], 0)
#' Y   <- tau * W + rnorm(n)
#'
#' cf  <- causal_forest(X, Y, W, num.trees = 500)
#' result <- causal_forest_summary(cf, X, feature_names = colnames(X))
#' result$ate
#' result$plot_cate_dist
#' }
#'
#' @importFrom stats sd quantile
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline geom_col
#'   labs theme_minimal
#' @export
causal_forest_summary <- function(cf,
                                   X,
                                   feature_names = NULL,
                                   n_groups      = 5,
                                   top_features  = 10,
                                   plot          = TRUE) {

  if (!requireNamespace("grf", quietly = TRUE)) {
    stop("Package 'grf' is required. Install with: install.packages('grf')",
         call. = FALSE)
  }

  if (!inherits(cf, "causal_forest")) {
    stop("`cf` must be a causal_forest object.", call. = FALSE)
  }

  if (is.null(feature_names)) {
    feature_names <- if (!is.null(colnames(X))) colnames(X) else
      paste0("X", seq_len(ncol(X)))
  }

  # --- ATE ---
  ate_res <- grf::average_treatment_effect(cf, target.sample = "all")
  z_crit  <- stats::qnorm(0.975)
  ate_df  <- data.frame(
    estimand  = "ATE",
    estimate  = ate_res["estimate"],
    std_error = ate_res["std.err"],
    t_stat    = ate_res["estimate"] / ate_res["std.err"],
    p_value   = 2 * stats::pnorm(-abs(ate_res["estimate"] / ate_res["std.err"])),
    ci_lower  = ate_res["estimate"] - z_crit * ate_res["std.err"],
    ci_upper  = ate_res["estimate"] + z_crit * ate_res["std.err"],
    row.names = NULL
  )

  # --- CATE predictions ---
  preds <- predict(cf, estimate.variance = TRUE)
  tau_hat <- preds$predictions

  cate_summary <- c(
    mean      = mean(tau_hat),
    sd        = stats::sd(tau_hat),
    min       = min(tau_hat),
    q25       = unname(stats::quantile(tau_hat, 0.25)),
    median    = unname(stats::quantile(tau_hat, 0.50)),
    q75       = unname(stats::quantile(tau_hat, 0.75)),
    max       = max(tau_hat),
    pct_pos   = mean(tau_hat > 0),
    pct_neg   = mean(tau_hat < 0)
  )

  # --- Feature importance ---
  varimp <- grf::variable_importance(cf)
  fi_df  <- data.frame(
    feature    = feature_names[seq_along(varimp)],
    importance = as.numeric(varimp),
    stringsAsFactors = FALSE
  )
  fi_df <- fi_df[order(fi_df$importance, decreasing = TRUE), ]
  fi_df <- fi_df[seq_len(min(top_features, nrow(fi_df))), ]
  fi_df$feature <- factor(fi_df$feature, levels = rev(fi_df$feature))

  # --- BLP / GATES analysis ---
  blp_res <- tryCatch({
    blp_analysis(
      cate_hat  = tau_hat,
      Y         = cf$Y.orig,
      W         = cf$W.orig,
      Y_hat     = cf$Y.hat,
      W_hat     = cf$W.hat,
      n_groups  = n_groups,
      run_gates = TRUE
    )
  }, error = function(e) NULL)

  out <- list(
    ate              = ate_df,
    cate_summary     = cate_summary,
    blp              = blp_res,
    feature_importance = fi_df,
    plot_cate_dist   = NULL,
    plot_importance  = NULL,
    plot_blp         = NULL,
    plot_gates       = NULL
  )

  if (plot) {
    # CATE distribution
    tau_df <- data.frame(tau = tau_hat)
    out$plot_cate_dist <- ggplot2::ggplot(tau_df, ggplot2::aes(x = tau)) +
      ggplot2::geom_histogram(bins = 40, fill = "#4393C3", alpha = 0.7,
                              color = "white") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::geom_vline(xintercept = ate_df$estimate,
                          color = "#D73027", linewidth = 1.2) +
      ggplot2::labs(
        x     = "Estimated CATE",
        y     = "Count",
        title = "Distribution of Conditional Average Treatment Effects",
        subtitle = sprintf(
          "ATE = %.4f (SE = %.4f, p = %.4f); %.1f%% positive, %.1f%% negative",
          ate_df$estimate, ate_df$std_error, ate_df$p_value,
          cate_summary["pct_pos"] * 100, cate_summary["pct_neg"] * 100
        )
      ) +
      ggplot2::theme_minimal(base_size = 12)

    # Feature importance
    out$plot_importance <- ggplot2::ggplot(fi_df,
      ggplot2::aes(x = feature, y = importance)) +
      ggplot2::geom_col(fill = "#4393C3", alpha = 0.8) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x     = NULL,
        y     = "Variable Importance",
        title = paste0("Top ", min(top_features, nrow(fi_df)),
                       " Most Important Features")
      ) +
      ggplot2::theme_minimal(base_size = 12)

    if (!is.null(blp_res)) {
      out$plot_blp   <- blp_res$blp_plot
      out$plot_gates <- blp_res$gates_plot
    }
  }

  invisible(out)
}
