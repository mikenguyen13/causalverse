#' Panel Data Diagnostic Tests
#'
#' Runs a battery of diagnostic tests on panel data: unit roots,
#' cross-sectional dependence, serial correlation, and heteroskedasticity.
#'
#' @param data A data frame in long format.
#' @param outcome Character. Name of the outcome variable.
#' @param unit_var Character. Name of the unit (panel) identifier.
#' @param time_var Character. Name of the time variable.
#' @param tests Character vector. Which tests to run. Options:
#'   \code{"unit_root"}, \code{"serial_corr"}, \code{"cross_dep"},
#'   \code{"heterosked"}. Default: all four.
#' @param plot Logical. Produce a 4-panel diagnostic plot. Default \code{TRUE}.
#'
#' @return An object of class \code{"panel_diagnostics"}: a named list with one
#'   element per requested test. Each element is a list with:
#'   \describe{
#'     \item{statistic}{Main test statistic.}
#'     \item{p_value}{Corresponding p-value.}
#'     \item{decision}{Character: \code{"Reject H0"} or \code{"Fail to reject H0"}.}
#'     \item{details}{Additional per-unit or supplementary results.}
#'   }
#'   When \code{plot = TRUE} the list also contains a \code{plot} element
#'   (a \code{ggplot2} object).
#'
#' @details
#' \strong{Unit root (IPS approximation):} ADF tests are run unit-by-unit via
#' \code{tseries::adf.test}. The Im-Pesaran-Shin (2003) panel statistic is
#' approximated by standardising the mean ADF t-statistic.  KPSS tests
#' (\code{tseries::kpss.test}) are also run as a confirmatory check.
#'
#' \strong{Serial correlation (Wooldridge 2002):} The outcome is first-
#' differenced within each unit. Residuals from an OLS of the differenced
#' outcome on unit dummies are tested for AR(1) autocorrelation: the
#' coefficient on the lagged residual should be \eqn{-0.5} under no serial
#' correlation; a t-test on that coefficient is reported.
#'
#' \strong{Cross-sectional dependence (Pesaran 2004):} Pairwise correlations
#' \eqn{\hat{\rho}_{ij}} of de-meaned residuals across units are computed. The
#' CD statistic \eqn{\sqrt{2T/(N(N-1))} \sum_{i<j} \hat{\rho}_{ij}} is
#' compared to a standard normal.
#'
#' \strong{Heteroskedasticity (Breusch-Pagan):} A pooled OLS of the outcome on
#' unit and time dummies is estimated; the Breusch-Pagan test is applied via
#' \code{lmtest::bptest} when available, otherwise a manual chi-squared
#' approximation is used.
#'
#' @references
#' Im, K. S., Pesaran, M. H., and Shin, Y. (2003). "Testing for unit roots in
#' heterogeneous panels." \emph{Journal of Econometrics}, 115(1), 53-74.
#'
#' Pesaran, M. H. (2004). "General diagnostic tests for cross section
#' dependence in panels." \emph{Cambridge Working Papers in Economics} 0435.
#'
#' Wooldridge, J. M. (2002). \emph{Econometric Analysis of Cross Section and
#' Panel Data}. MIT Press.
#'
#' @examples
#' \dontrun{
#' # Simulate a balanced panel
#' set.seed(42)
#' N <- 20; TT <- 10
#' panel <- data.frame(
#'   unit = rep(1:N, each = TT),
#'   time = rep(1:TT, times = N),
#'   y    = rnorm(N * TT)
#' )
#' result <- panel_diagnostics(panel, outcome = "y",
#'                              unit_var = "unit", time_var = "time")
#' print(result)
#' }
#'
#' @importFrom stats lm residuals fitted sd pnorm pt as.formula
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline geom_tile
#'   geom_point geom_smooth labs theme_minimal scale_fill_gradient2
#'   facet_wrap
#' @export
panel_diagnostics <- function(data,
                               outcome,
                               unit_var,
                               time_var,
                               tests = c("unit_root", "serial_corr",
                                         "cross_dep", "heterosked"),
                               plot  = TRUE) {

  # ------------------------------------------------------------------
  # Input validation
  # ------------------------------------------------------------------
  tests <- match.arg(tests, several.ok = TRUE)

  req_cols <- c(outcome, unit_var, time_var)
  missing  <- setdiff(req_cols, names(data))
  if (length(missing) > 0L) {
    stop("Columns not found in data: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  data     <- data[order(data[[unit_var]], data[[time_var]]), ]
  units    <- unique(data[[unit_var]])
  times    <- unique(data[[time_var]])
  N        <- length(units)
  TT       <- length(times)

  if (N < 2L) stop("Panel must have at least 2 units.", call. = FALSE)
  if (TT < 4L) stop("Panel must have at least 4 time periods.", call. = FALSE)

  results <- list()

  # ------------------------------------------------------------------
  # Helper: extract time series for one unit
  # ------------------------------------------------------------------
  unit_ts <- function(uid) {
    sub <- data[data[[unit_var]] == uid, ]
    sub <- sub[order(sub[[time_var]]), ]
    sub[[outcome]]
  }

  # ==================================================================
  # 1. Unit Root Tests (ADF + KPSS, IPS panel statistic)
  # ==================================================================
  if ("unit_root" %in% tests) {

    if (!requireNamespace("tseries", quietly = TRUE)) {
      message("Package 'tseries' is required for unit root tests. ",
              "Install with: install.packages('tseries')")
      results$unit_root <- list(
        statistic = NA_real_, p_value = NA_real_,
        decision  = "Test skipped (tseries not installed)",
        details   = NULL
      )
    } else {

      adf_stats <- numeric(N)
      adf_pvs   <- numeric(N)
      kpss_pvs  <- numeric(N)
      names(adf_stats) <- names(adf_pvs) <- names(kpss_pvs) <- as.character(units)

      for (i in seq_along(units)) {
        y_i <- unit_ts(units[i])
        # ADF
        adf_res <- tryCatch(
          tseries::adf.test(y_i, k = min(floor((TT - 1)^(1/3)), TT - 3L)),
          error = function(e) NULL
        )
        if (!is.null(adf_res)) {
          adf_stats[i] <- adf_res$statistic
          adf_pvs[i]   <- adf_res$p.value
        } else {
          adf_stats[i] <- NA_real_
          adf_pvs[i]   <- NA_real_
        }
        # KPSS
        kpss_res <- tryCatch(
          tseries::kpss.test(y_i, null = "Level"),
          error = function(e) NULL
        )
        kpss_pvs[i] <- if (!is.null(kpss_res)) kpss_res$p.value else NA_real_
      }

      # Im-Pesaran-Shin panel statistic (approximation)
      # W_bar = (mean(t_ADF) - E[t_ADF]) / sqrt(Var[t_ADF])
      # Asymptotic moments for ADF(0) from IPS (2003) Table 1, T->infinity approx:
      # E[t] ~ -1.504, Var[t] ~ 0.852 (lag=0, no trend)
      mu_ips  <- -1.504
      sig_ips <- sqrt(0.852)
      valid_t <- adf_stats[!is.na(adf_stats)]
      N_valid <- length(valid_t)

      if (N_valid > 0L) {
        W_bar   <- sqrt(N_valid) * (mean(valid_t) - mu_ips) / sig_ips
        ips_pv  <- 2 * stats::pnorm(-abs(W_bar))
      } else {
        W_bar  <- NA_real_
        ips_pv <- NA_real_
      }

      pct_reject_adf  <- mean(adf_pvs  < 0.05, na.rm = TRUE) * 100
      pct_reject_kpss <- mean(kpss_pvs < 0.05, na.rm = TRUE) * 100

      results$unit_root <- list(
        statistic = W_bar,
        p_value   = ips_pv,
        decision  = if (!is.na(ips_pv) && ips_pv < 0.05)
                      "Reject H0 (evidence against unit root)"
                    else
                      "Fail to reject H0 (unit root not ruled out)",
        details   = list(
          adf_statistics    = adf_stats,
          adf_p_values      = adf_pvs,
          kpss_p_values     = kpss_pvs,
          pct_reject_adf    = pct_reject_adf,
          pct_reject_kpss   = pct_reject_kpss,
          ips_W_bar         = W_bar,
          ips_p_value       = ips_pv
        )
      )
    }
  }

  # ==================================================================
  # 2. Serial Correlation (Wooldridge 2002)
  # ==================================================================
  if ("serial_corr" %in% tests) {

    # First-difference within each unit
    diff_rows <- lapply(units, function(uid) {
      sub <- data[data[[unit_var]] == uid, ]
      sub <- sub[order(sub[[time_var]]), ]
      y   <- sub[[outcome]]
      if (length(y) < 3L) return(NULL)
      data.frame(
        unit   = uid,
        time   = sub[[time_var]][-1L],
        dy     = diff(y),
        stringsAsFactors = FALSE
      )
    })
    diff_df <- do.call(rbind, Filter(Negate(is.null), diff_rows))

    if (nrow(diff_df) < 4L) {
      results$serial_corr <- list(
        statistic = NA_real_, p_value = NA_real_,
        decision  = "Insufficient data for serial correlation test",
        details   = NULL
      )
    } else {
      # Regress first-differenced outcome on unit fixed effects,
      # then test if residuals are AR(1) with rho = -0.5
      diff_df$unit_f <- as.factor(diff_df$unit)
      ols_fd <- stats::lm(dy ~ unit_f - 1, data = diff_df)
      diff_df$resid_fd <- stats::residuals(ols_fd)

      # Lag residuals within each unit
      lag_rows <- lapply(split(diff_df, diff_df$unit), function(sub) {
        sub <- sub[order(sub$time), ]
        if (nrow(sub) < 2L) return(NULL)
        data.frame(
          e    = sub$resid_fd[-1L],
          e_l1 = sub$resid_fd[-nrow(sub)],
          stringsAsFactors = FALSE
        )
      })
      lag_df <- do.call(rbind, Filter(Negate(is.null), lag_rows))

      if (nrow(lag_df) < 3L) {
        results$serial_corr <- list(
          statistic = NA_real_, p_value = NA_real_,
          decision  = "Insufficient lagged observations",
          details   = NULL
        )
      } else {
        ar1_fit <- stats::lm(e ~ e_l1, data = lag_df)
        ar1_sum <- summary(ar1_fit)$coefficients
        # Under H0 of no serial corr, rho = -0.5 in the first-difference model
        rho_hat <- ar1_sum["e_l1", "Estimate"]
        rho_se  <- ar1_sum["e_l1", "Std. Error"]
        t_wool  <- (rho_hat - (-0.5)) / rho_se
        df_wool <- nrow(lag_df) - 2L
        pv_wool <- 2 * stats::pt(-abs(t_wool), df = df_wool)

        results$serial_corr <- list(
          statistic = t_wool,
          p_value   = pv_wool,
          decision  = if (pv_wool < 0.05)
                        "Reject H0 (serial correlation present)"
                      else
                        "Fail to reject H0 (no evidence of serial correlation)",
          details   = list(
            rho_hat  = rho_hat,
            rho_se   = rho_se,
            df       = df_wool,
            t_stat   = t_wool
          )
        )
      }
    }
  }

  # ==================================================================
  # 3. Cross-Sectional Dependence (Pesaran 2004 CD test)
  # ==================================================================
  if ("cross_dep" %in% tests) {

    # Obtain within-unit demeaned residuals (pooled OLS residuals)
    fmla  <- stats::as.formula(
      paste(outcome, "~", "as.factor(", unit_var, ") + as.factor(", time_var, ")")
    )
    pool_fit <- tryCatch(
      stats::lm(fmla, data = data),
      error = function(e) NULL
    )

    if (is.null(pool_fit)) {
      results$cross_dep <- list(
        statistic = NA_real_, p_value = NA_real_,
        decision  = "CD test failed (pooled model could not be fitted)",
        details   = NULL
      )
    } else {
      data$.resid_pool <- stats::residuals(pool_fit)

      # Build residual matrix (T x N)
      resid_mat <- matrix(NA_real_, nrow = TT, ncol = N)
      colnames(resid_mat) <- as.character(units)
      rownames(resid_mat) <- as.character(times)

      for (i in seq_along(units)) {
        sub_i   <- data[data[[unit_var]] == units[i], ]
        sub_i   <- sub_i[order(sub_i[[time_var]]), ]
        t_idx   <- match(as.character(sub_i[[time_var]]), rownames(resid_mat))
        resid_mat[t_idx, i] <- sub_i$.resid_pool
      }

      # Pairwise correlations (use common time periods)
      rho_sum <- 0
      n_pairs <- 0L
      rho_mat <- matrix(NA_real_, N, N)

      for (i in seq_len(N - 1L)) {
        for (j in (i + 1L):N) {
          obs   <- complete.cases(cbind(resid_mat[, i], resid_mat[, j]))
          T_ij  <- sum(obs)
          if (T_ij < 3L) next
          rho_ij         <- stats::cor(resid_mat[obs, i], resid_mat[obs, j])
          rho_mat[i, j]  <- rho_mat[j, i] <- rho_ij
          rho_sum        <- rho_sum + T_ij * rho_ij
          n_pairs        <- n_pairs + 1L
        }
      }

      if (n_pairs == 0L) {
        CD_stat <- NA_real_
        CD_pv   <- NA_real_
      } else {
        # Pesaran (2004): CD = sqrt(2/(N*(N-1))) * sum_{i<j} sqrt(T_ij) * rho_ij
        # Approximation using mean T:
        T_bar   <- TT
        CD_stat <- sqrt(2 * T_bar / (N * (N - 1))) * rho_sum
        CD_pv   <- 2 * stats::pnorm(-abs(CD_stat))
      }

      diag(rho_mat) <- 1

      results$cross_dep <- list(
        statistic = CD_stat,
        p_value   = CD_pv,
        decision  = if (!is.na(CD_pv) && CD_pv < 0.05)
                      "Reject H0 (cross-sectional dependence detected)"
                    else
                      "Fail to reject H0 (no significant cross-sectional dependence)",
        details   = list(
          rho_matrix   = rho_mat,
          n_pairs      = n_pairs,
          mean_abs_rho = mean(abs(rho_mat[upper.tri(rho_mat)]), na.rm = TRUE)
        )
      )
      # Clean up helper column
      data$.resid_pool <- NULL
    }
  }

  # ==================================================================
  # 4. Heteroskedasticity (Breusch-Pagan)
  # ==================================================================
  if ("heterosked" %in% tests) {

    fmla_pool <- stats::as.formula(
      paste(outcome, "~",
            "as.factor(", unit_var, ") + as.factor(", time_var, ")")
    )
    pool_fit2 <- tryCatch(
      stats::lm(fmla_pool, data = data),
      error = function(e) NULL
    )

    if (is.null(pool_fit2)) {
      results$heterosked <- list(
        statistic = NA_real_, p_value = NA_real_,
        decision  = "Test failed (model could not be fitted)",
        details   = NULL
      )
    } else if (requireNamespace("lmtest", quietly = TRUE)) {
      bp <- tryCatch(
        lmtest::bptest(pool_fit2),
        error = function(e) NULL
      )
      if (!is.null(bp)) {
        results$heterosked <- list(
          statistic = unname(bp$statistic),
          p_value   = bp$p.value,
          decision  = if (bp$p.value < 0.05)
                        "Reject H0 (heteroskedasticity detected)"
                      else
                        "Fail to reject H0 (homoskedasticity not rejected)",
          details   = list(method = bp$method, df = bp$parameter)
        )
      } else {
        results$heterosked <- list(
          statistic = NA_real_, p_value = NA_real_,
          decision  = "Breusch-Pagan test failed",
          details   = NULL
        )
      }
    } else {
      # Manual Breusch-Pagan: regress squared residuals on fitted values
      message("Package 'lmtest' not installed; using manual Breusch-Pagan approximation.")
      e2   <- stats::residuals(pool_fit2)^2
      yhat <- stats::fitted(pool_fit2)
      bp_lm  <- stats::lm(e2 ~ yhat)
      bp_sum <- summary(bp_lm)
      n_obs  <- length(e2)
      # LM = n * R^2
      LM   <- n_obs * bp_sum$r.squared
      bp_pv <- stats::pchisq(LM, df = 1L, lower.tail = FALSE)

      results$heterosked <- list(
        statistic = LM,
        p_value   = bp_pv,
        decision  = if (bp_pv < 0.05)
                      "Reject H0 (heteroskedasticity detected)"
                    else
                      "Fail to reject H0 (homoskedasticity not rejected)",
        details   = list(
          method = "Manual Breusch-Pagan (LM = n * R^2 of e^2 ~ fitted)",
          df     = 1L
        )
      )
    }
  }

  # ==================================================================
  # 5. Diagnostic plots
  # ==================================================================
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {

    plot_list <- list()

    # --- 5a. Unit root: ADF t-statistic histogram ---
    if ("unit_root" %in% tests && !is.null(results$unit_root$details)) {
      adf_df <- data.frame(
        adf_t = results$unit_root$details$adf_statistics,
        stringsAsFactors = FALSE
      )
      p_ur <- ggplot2::ggplot(adf_df, ggplot2::aes(x = adf_t)) +
        ggplot2::geom_histogram(bins = min(20L, N), fill = "#2166AC",
                                color = "white", alpha = 0.8) +
        ggplot2::geom_vline(xintercept = -2.86, linetype = "dashed",
                            color = "red") +
        ggplot2::labs(
          title   = "Unit Root: ADF t-statistics by unit",
          x       = "ADF t-statistic",
          y       = "Count",
          caption = "Red dashed line: 5% critical value (-2.86)"
        ) +
        ama_theme(base_size = 11)
      plot_list$unit_root_hist <- p_ur
    }

    # --- 5b. Serial correlation: ACF of pooled FD residuals ---
    if ("serial_corr" %in% tests) {
      diff_rows2 <- lapply(units, function(uid) {
        sub <- data[data[[unit_var]] == uid, ]
        sub <- sub[order(sub[[time_var]]), ]
        y   <- sub[[outcome]]
        if (length(y) < 2L) return(NULL)
        data.frame(dy = diff(y), stringsAsFactors = FALSE)
      })
      diff_all <- do.call(rbind, Filter(Negate(is.null), diff_rows2))$dy
      if (length(diff_all) >= 4L) {
        acf_obj  <- stats::acf(diff_all, plot = FALSE, lag.max = min(10L, TT - 1L))
        acf_df   <- data.frame(
          lag_val = as.numeric(acf_obj$lag[-1L]),
          acf_val = as.numeric(acf_obj$acf[-1L])
        )
        ci_bound <- qnorm(0.975) / sqrt(length(diff_all))
        p_acf <- ggplot2::ggplot(acf_df, ggplot2::aes(x = lag_val, y = acf_val)) +
          ggplot2::geom_col(fill = "#4DAF4A", alpha = 0.7, width = 0.4) +
          ggplot2::geom_hline(yintercept = c(-ci_bound, ci_bound),
                              linetype = "dashed", color = "red") +
          ggplot2::geom_hline(yintercept = 0, color = "black") +
          ggplot2::labs(
            title   = "Serial Correlation: ACF of first-differenced residuals",
            x       = "Lag",
            y       = "ACF",
            caption = "Red dashed lines: 95% confidence bounds"
          ) +
          ama_theme(base_size = 11)
        plot_list$acf_plot <- p_acf
      }
    }

    # --- 5c. Cross-sectional dependence: pairwise correlation heatmap ---
    if ("cross_dep" %in% tests && !is.null(results$cross_dep$details)) {
      rho_m <- results$cross_dep$details$rho_matrix
      if (!is.null(rho_m) && !all(is.na(rho_m))) {
        unit_labels <- as.character(units)
        # Subsample if too many units
        max_show <- 30L
        if (N > max_show) {
          idx_show   <- round(seq(1, N, length.out = max_show))
          rho_m      <- rho_m[idx_show, idx_show, drop = FALSE]
          unit_labels <- unit_labels[idx_show]
        }
        rho_long <- data.frame(
          unit_i = rep(unit_labels, times = length(unit_labels)),
          unit_j = rep(unit_labels, each  = length(unit_labels)),
          rho    = as.vector(rho_m),
          stringsAsFactors = FALSE
        )
        rho_long$unit_i <- factor(rho_long$unit_i, levels = unit_labels)
        rho_long$unit_j <- factor(rho_long$unit_j, levels = unit_labels)

        p_cd <- ggplot2::ggplot(rho_long, ggplot2::aes(x = unit_i, y = unit_j,
                                                        fill = rho)) +
          ggplot2::geom_tile() +
          ggplot2::scale_fill_gradient2(
            low      = "#D73027",
            mid      = "white",
            high     = "#2166AC",
            midpoint = 0,
            na.value = "gray90",
            name     = expression(hat(rho))
          ) +
          ggplot2::labs(
            title = "Cross-Sectional Dependence: Pairwise residual correlations",
            x = "Unit", y = "Unit"
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, hjust = 1,
                                                 size = 7),
            axis.text.y = ggplot2::element_text(size = 7)
          )
        plot_list$cd_heatmap <- p_cd
      }
    }

    # --- 5d. Heteroskedasticity: residual vs. fitted ---
    if ("heterosked" %in% tests) {
      fmla_pool3 <- stats::as.formula(
        paste(outcome, "~",
              "as.factor(", unit_var, ") + as.factor(", time_var, ")")
      )
      pool_fit3 <- tryCatch(
        stats::lm(fmla_pool3, data = data),
        error = function(e) NULL
      )
      if (!is.null(pool_fit3)) {
        diag_df <- data.frame(
          fitted_vals   = stats::fitted(pool_fit3),
          resid_vals    = stats::residuals(pool_fit3),
          stringsAsFactors = FALSE
        )
        p_hsk <- ggplot2::ggplot(diag_df,
                                  ggplot2::aes(x = fitted_vals, y = resid_vals)) +
          ggplot2::geom_point(alpha = 0.3, size = 1.2, color = "#666666") +
          ggplot2::geom_smooth(method = "loess", se = FALSE,
                               color = "#D73027", linewidth = 0.8,
                               formula = y ~ x) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                              color = "gray50") +
          ggplot2::labs(
            title   = "Heteroskedasticity: Residuals vs. Fitted",
            x       = "Fitted values",
            y       = "Residuals",
            caption = "Loess smoother in red"
          ) +
          ama_theme(base_size = 11)
        plot_list$resid_fitted <- p_hsk
      }
    }

    # Combine available panels using patchwork or return list
    if (requireNamespace("patchwork", quietly = TRUE) && length(plot_list) > 1L) {
      combined <- Reduce(`+`, plot_list)
      results$plot <- patchwork::wrap_plots(plot_list, ncol = 2L)
    } else if (length(plot_list) == 1L) {
      results$plot <- plot_list[[1L]]
    } else {
      results$plot <- plot_list
    }
  }

  class(results) <- c("panel_diagnostics", "list")
  results
}


# ------------------------------------------------------------------
# S3 print method
# ------------------------------------------------------------------

#' Print method for panel_diagnostics objects
#'
#' @param x An object of class \code{panel_diagnostics}.
#' @param ... Ignored.
#' @keywords internal
#' @export
print.panel_diagnostics <- function(x, ...) {

  cat("=== Panel Data Diagnostic Tests ===\n\n")

  test_labels <- c(
    unit_root   = "1. Unit Root (Im-Pesaran-Shin panel ADF)",
    serial_corr = "2. Serial Correlation (Wooldridge 2002)",
    cross_dep   = "3. Cross-Sectional Dependence (Pesaran 2004 CD)",
    heterosked  = "4. Heteroskedasticity (Breusch-Pagan)"
  )

  for (tname in names(test_labels)) {
    if (!tname %in% names(x)) next
    res <- x[[tname]]
    cat(test_labels[tname], "\n")
    cat(strrep("-", nchar(test_labels[tname])), "\n")
    if (!is.na(res$statistic)) {
      cat(sprintf("  Statistic : %.4f\n", res$statistic))
      cat(sprintf("  P-value   : %.4f\n", res$p_value))
    }
    cat(sprintf("  Decision  : %s\n", res$decision))
    # Extra details
    if (tname == "unit_root" && !is.null(res$details)) {
      cat(sprintf("  %% units rejecting ADF H0 : %.1f%%\n",
                  res$details$pct_reject_adf))
      cat(sprintf("  %% units rejecting KPSS H0: %.1f%%\n",
                  res$details$pct_reject_kpss))
    }
    if (tname == "cross_dep" && !is.null(res$details)) {
      cat(sprintf("  Mean |rho|: %.4f\n", res$details$mean_abs_rho))
    }
    cat("\n")
  }

  invisible(x)
}
