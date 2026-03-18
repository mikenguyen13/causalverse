#' Comprehensive RD Assumption Tests
#'
#' Runs a battery of standard robustness and assumption tests for regression
#' discontinuity designs: density continuity (McCrary/rddensity), covariate
#' balance at the cutoff, bandwidth sensitivity, placebo cutoffs, and
#' polynomial order sensitivity.
#'
#' @param data A data frame.
#' @param outcome Character. Outcome variable name.
#' @param running_var Character. Running (forcing) variable name.
#' @param cutoff Numeric. RD cutoff. Default \code{0}.
#' @param covariates Character vector or \code{NULL}. Covariates to test for
#'   balance at the cutoff.
#' @param bw_seq Numeric vector. Bandwidth multipliers to test in sensitivity
#'   analysis. Default \code{c(0.5, 0.75, 1, 1.25, 1.5)}.
#' @param placebo_cutoffs Numeric vector or \code{NULL}. Placebo cutoff values
#'   (should be far from \code{cutoff}). If \code{NULL}, auto-selected.
#' @param poly_orders Integer vector. Polynomial orders to test. Default
#'   \code{1:4}.
#' @param kernel Character. RD kernel. Default \code{"triangular"}.
#' @param verbose Logical. Whether to print test results. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{density_test}{Output from \code{rddensity::rddensity()}.}
#'     \item{covariate_balance}{Data frame of RD estimates on each covariate.}
#'     \item{bandwidth_sensitivity}{Data frame of estimates at different
#'       bandwidths.}
#'     \item{placebo_results}{Data frame of placebo cutoff estimates.}
#'     \item{poly_sensitivity}{Data frame of estimates by polynomial order.}
#'     \item{summary_report}{Character. Text summary of all tests.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(rdrobust)
#' data(rdrobust_RDsenate)
#' result <- rd_assumption_tests(
#'   data        = rdrobust_RDsenate,
#'   outcome     = "vote",
#'   running_var = "margin"
#' )
#' cat(result$summary_report)
#' }
#'
#' @importFrom stats as.formula
#' @export
rd_assumption_tests <- function(data,
                                outcome,
                                running_var,
                                cutoff           = 0,
                                covariates       = NULL,
                                bw_seq           = c(0.5, 0.75, 1, 1.25, 1.5),
                                placebo_cutoffs  = NULL,
                                poly_orders      = 1:4,
                                kernel           = "triangular",
                                verbose          = TRUE) {

  req_cols <- c(outcome, running_var)
  missing  <- setdiff(req_cols, names(data))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  Y <- data[[outcome]]
  X <- data[[running_var]] - cutoff  # Center at cutoff

  results <- list()
  report  <- character(0)

  # --- 1. Density test (McCrary/rddensity) ---
  if (requireNamespace("rddensity", quietly = TRUE)) {
    dens <- tryCatch(
      rddensity::rddensity(X = X, c = 0),
      error = function(e) NULL
    )
    results$density_test <- dens
    if (!is.null(dens)) {
      s <- summary(dens)
      report <- c(report,
        "=== 1. Density Continuity Test (McCrary/rddensity) ===",
        sprintf("T-stat = %.4f, p-value = %.4f",
                dens$test$t_jk, dens$test$p_jk),
        sprintf("Interpretation: %s\n",
                ifelse(dens$test$p_jk > 0.05,
                  "PASS: No evidence of manipulation (p > 0.05)",
                  "FAIL: Possible manipulation (p <= 0.05)"))
      )
    }
  } else {
    report <- c(report, "Density test skipped (rddensity not installed).\n")
    results$density_test <- NULL
  }

  # --- 2. Covariate balance at cutoff ---
  if (!is.null(covariates) && requireNamespace("rdrobust", quietly = TRUE)) {
    cov_rows <- lapply(covariates, function(v) {
      tryCatch({
        rd_cov <- rdrobust::rdrobust(
          y = data[[v]], x = X, c = 0, kernel = kernel
        )
        data.frame(
          covariate = v,
          estimate  = rd_cov$coef[1],
          se        = rd_cov$se[1],
          p_value   = rd_cov$pv[1],
          passes    = rd_cov$pv[1] > 0.05,
          stringsAsFactors = FALSE
        )
      }, error = function(e) {
        data.frame(covariate = v, estimate = NA, se = NA,
                   p_value = NA, passes = NA, stringsAsFactors = FALSE)
      })
    })
    cov_df <- do.call(rbind, cov_rows)
    results$covariate_balance <- cov_df

    n_fail <- sum(!cov_df$passes, na.rm = TRUE)
    report <- c(report,
      "=== 2. Covariate Balance at Cutoff ===",
      sprintf("Tested %d covariates. %d fail (p <= 0.05).",
              nrow(cov_df), n_fail),
      ifelse(n_fail == 0, "PASS: All covariates balanced.\n",
             "WARNING: Some covariates imbalanced at cutoff.\n")
    )
  } else {
    results$covariate_balance <- NULL
  }

  # --- 3. Bandwidth sensitivity ---
  if (requireNamespace("rdrobust", quietly = TRUE)) {
    # Get default bandwidth
    main_rd <- tryCatch(
      rdrobust::rdrobust(y = Y, x = X, c = 0, kernel = kernel),
      error = function(e) NULL
    )
    if (!is.null(main_rd)) {
      h0 <- main_rd$bws[1, 1]
      bw_rows <- lapply(bw_seq, function(mult) {
        h_use <- h0 * mult
        rd_h <- tryCatch(
          rdrobust::rdrobust(y = Y, x = X, c = 0, h = h_use, kernel = kernel),
          error = function(e) NULL
        )
        if (is.null(rd_h)) return(NULL)
        data.frame(
          bw_multiplier = mult,
          bandwidth     = h_use,
          estimate      = rd_h$coef[1],
          se            = rd_h$se[1],
          p_value       = rd_h$pv[1],
          stringsAsFactors = FALSE
        )
      })
      bw_df <- do.call(rbind, Filter(Negate(is.null), bw_rows))
      results$bandwidth_sensitivity <- bw_df

      report <- c(report,
        "=== 3. Bandwidth Sensitivity ===",
        sprintf("Default bandwidth: %.4f", h0),
        "Estimates across bandwidths:",
        paste(
          sprintf("  %.2fx bw: est=%.4f (SE=%.4f, p=%.3f)",
                  bw_df$bw_multiplier, bw_df$estimate,
                  bw_df$se, bw_df$p_value),
          collapse = "\n"
        ), "\n"
      )
    }
  }

  # --- 4. Placebo cutoffs ---
  if (requireNamespace("rdrobust", quietly = TRUE)) {
    if (is.null(placebo_cutoffs)) {
      # Auto-select: quartiles of the running variable away from true cutoff
      q_vals <- stats::quantile(X, c(0.25, 0.75), na.rm = TRUE)
      placebo_cutoffs <- q_vals[abs(q_vals) > diff(range(X, na.rm = TRUE)) * 0.15]
    }
    if (length(placebo_cutoffs) > 0) {
      pc_rows <- lapply(placebo_cutoffs, function(pc) {
        # Use only observations on one side of the true cutoff to avoid contamination
        idx <- if (pc > 0) X > 0 else X < 0
        if (sum(idx) < 10) return(NULL)
        rd_pc <- tryCatch(
          rdrobust::rdrobust(y = Y[idx], x = X[idx], c = pc, kernel = kernel),
          error = function(e) NULL
        )
        if (is.null(rd_pc)) return(NULL)
        data.frame(
          placebo_cutoff = pc,
          estimate       = rd_pc$coef[1],
          se             = rd_pc$se[1],
          p_value        = rd_pc$pv[1],
          passes         = rd_pc$pv[1] > 0.05,
          stringsAsFactors = FALSE
        )
      })
      pc_df <- do.call(rbind, Filter(Negate(is.null), pc_rows))
      results$placebo_results <- pc_df
      if (!is.null(pc_df)) {
        n_fail_p <- sum(!pc_df$passes, na.rm = TRUE)
        report   <- c(report,
          "=== 4. Placebo Cutoff Tests ===",
          sprintf("%d cutoffs tested. %d significant (p <= 0.05).",
                  nrow(pc_df), n_fail_p),
          ifelse(n_fail_p == 0, "PASS: No significant placebo effects.\n",
                 "WARNING: Some placebo cutoffs significant.\n")
        )
      }
    }
  }

  # --- 5. Polynomial order sensitivity ---
  if (requireNamespace("rdrobust", quietly = TRUE)) {
    poly_rows <- lapply(poly_orders, function(p) {
      rd_p <- tryCatch(
        rdrobust::rdrobust(y = Y, x = X, c = 0, p = p, kernel = kernel),
        error = function(e) NULL
      )
      if (is.null(rd_p)) return(NULL)
      data.frame(
        poly_order = p,
        estimate   = rd_p$coef[1],
        se         = rd_p$se[1],
        p_value    = rd_p$pv[1],
        stringsAsFactors = FALSE
      )
    })
    poly_df <- do.call(rbind, Filter(Negate(is.null), poly_rows))
    results$poly_sensitivity <- poly_df
    report <- c(report,
      "=== 5. Polynomial Order Sensitivity ===",
      paste(
        sprintf("  p=%d: est=%.4f (SE=%.4f, pval=%.3f)",
                poly_df$poly_order, poly_df$estimate,
                poly_df$se, poly_df$p_value),
        collapse = "\n"
      ), "\n"
    )
  }

  results$summary_report <- paste(report, collapse = "\n")

  if (verbose) cat(results$summary_report)

  invisible(results)
}
