#' Dose-Response Curve Estimation for Continuous Treatments
#'
#' Estimates and visualises the average potential outcome E\[Y(d)\] as a
#' function of a continuous treatment dose *d*. Three estimation strategies
#' are available: inverse-probability weighting with a generalised propensity
#' score (IPW), binned OLS, and a generalised additive model (GAM). Returns
#' the estimated curve with 95% confidence intervals and a ggplot2 object.
#'
#' @title Dose-Response Curve for Continuous Treatment
#'
#' @description Estimates the dose-response function for a continuous treatment
#'   using one of three methods and plots the resulting curve with a shaded
#'   95% CI ribbon. The IPW approach follows Hirano & Imbens (2004).
#'
#' @param data A data frame.
#' @param outcome Character. Name of the continuous outcome variable.
#' @param treatment Character. Name of the continuous treatment variable.
#' @param covariates Character vector. Names of pre-treatment covariates to
#'   adjust for. `NULL` for unadjusted estimates.
#' @param n_bins Integer. Number of bins for the treatment axis. Default `20`.
#' @param method Character. Estimation method: `"ipw"` (generalised propensity
#'   score weighting), `"ols"` (binned local means), or `"gam"` (GAM via
#'   \pkg{mgcv}, if available). Default `"ipw"`.
#' @param plot Logical. Whether to print and return the plot. Default `TRUE`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`curve_df`}{Data frame with columns `dose_mid`, `estimate`,
#'       `ci_lo`, `ci_hi`, `n` (observations in each bin).}
#'     \item{`plot`}{A ggplot2 object, or `NULL` if `plot = FALSE`.}
#'   }
#'
#' @references
#' Hirano, K., & Imbens, G. W. (2004). The propensity score with continuous
#' treatments. In A. Gelman & X.-L. Meng (Eds.), *Applied Bayesian Modeling
#' and Causal Inference from Incomplete-Data Perspectives*. Wiley.
#'
#' @examples
#' set.seed(123)
#' n   <- 600
#' age <- rnorm(n, 35, 10)
#' # Continuous dose: hours of training (0-40)
#' dose <- pmax(0, 10 + 3 * (age - 35) / 10 + rnorm(n, 0, 8))
#' y    <- 2000 + 80 * dose - 1.5 * dose^2 + 50 * age + rnorm(n, 0, 500)
#' df   <- data.frame(earnings = y, training = dose, age = age)
#'
#' result <- dose_response_curve(
#'   data       = df,
#'   outcome    = "earnings",
#'   treatment  = "training",
#'   covariates = "age",
#'   n_bins     = 15,
#'   method     = "ols"
#' )
#' result$plot
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#'   labs scale_color_manual scale_fill_manual theme
#' @importFrom stats lm predict dnorm sd model.matrix
#' @export
dose_response_curve <- function(data,
                                 outcome,
                                 treatment,
                                 covariates = NULL,
                                 n_bins     = 20,
                                 method     = c("ipw", "ols", "gam"),
                                 plot       = TRUE) {

  method <- match.arg(method)

  stopifnot(is.data.frame(data))
  for (v in c(outcome, treatment)) {
    if (!v %in% names(data)) stop(sprintf("Column '%s' not found in data.", v))
  }
  if (!is.null(covariates)) {
    miss <- setdiff(covariates, names(data))
    if (length(miss) > 0) stop("Covariates not found: ", paste(miss, collapse = ", "))
  }

  # Drop rows with NA in key variables
  keep_vars <- c(outcome, treatment, covariates)
  data      <- data[stats::complete.cases(data[, keep_vars, drop = FALSE]), ]

  d_vec <- data[[treatment]]
  y_vec <- data[[outcome]]
  n_obs <- nrow(data)

  if (n_obs < 20) stop("Too few observations after removing missing values.")

  # ---- Bin the treatment variable ------------------------------------------
  breaks    <- seq(min(d_vec), max(d_vec), length.out = n_bins + 1)
  bin_idx   <- cut(d_vec, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  bin_mids  <- (breaks[-length(breaks)] + breaks[-1]) / 2

  # ---- Estimation ----------------------------------------------------------
  if (method == "ols") {
    # Binned OLS: regress outcome on bin-mean dose ± covariates
    curve_list <- lapply(seq_len(n_bins), function(b) {
      in_bin <- which(bin_idx == b)
      if (length(in_bin) < 2) return(NULL)
      y_b <- y_vec[in_bin]
      mn  <- mean(y_b)
      se  <- stats::sd(y_b) / sqrt(length(y_b))
      data.frame(
        dose_mid = bin_mids[b],
        estimate = mn,
        ci_lo    = mn - 1.96 * se,
        ci_hi    = mn + 1.96 * se,
        n        = length(y_b),
        stringsAsFactors = FALSE
      )
    })
    curve_df <- do.call(rbind, Filter(Negate(is.null), curve_list))

  } else if (method == "ipw") {
    # Generalised propensity score via normal linear regression of D on X
    if (!is.null(covariates) && length(covariates) > 0) {
      fml_gps <- stats::as.formula(
        paste(treatment, "~", paste(covariates, collapse = " + "))
      )
      gps_fit <- stats::lm(fml_gps, data = data)
      d_hat   <- stats::fitted(gps_fit)
      sig_hat <- stats::sd(stats::resid(gps_fit))
    } else {
      d_hat   <- rep(mean(d_vec), n_obs)
      sig_hat <- stats::sd(d_vec)
    }
    # GPS: f(D | X) = dnorm(D, d_hat, sig_hat)
    gps_vals <- stats::dnorm(d_vec, mean = d_hat, sd = sig_hat)
    # Marginal: f(D) = dnorm(D, mean(D), sd(D))
    marg_d   <- stats::dnorm(d_vec, mean = mean(d_vec), sd = stats::sd(d_vec))
    ipw_wts  <- marg_d / (gps_vals + 1e-12)

    # Trim extreme weights (Winsorise at 99th percentile)
    wt_cap  <- stats::quantile(ipw_wts, 0.99)
    ipw_wts <- pmin(ipw_wts, wt_cap)

    # Weighted bin means
    curve_list <- lapply(seq_len(n_bins), function(b) {
      in_bin <- which(bin_idx == b)
      if (length(in_bin) < 2) return(NULL)
      y_b  <- y_vec[in_bin]
      w_b  <- ipw_wts[in_bin]
      mn   <- stats::weighted.mean(y_b, w_b)
      # Sandwich SE approximation
      resid_sq <- (y_b - mn)^2
      se_sq    <- sum(w_b^2 * resid_sq) / (sum(w_b))^2
      se       <- sqrt(se_sq)
      data.frame(
        dose_mid = bin_mids[b],
        estimate = mn,
        ci_lo    = mn - 1.96 * se,
        ci_hi    = mn + 1.96 * se,
        n        = length(y_b),
        stringsAsFactors = FALSE
      )
    })
    curve_df <- do.call(rbind, Filter(Negate(is.null), curve_list))

  } else {
    # GAM
    if (!requireNamespace("mgcv", quietly = TRUE)) {
      message("Package 'mgcv' not available; falling back to method = 'ols'.")
      return(dose_response_curve(
        data = data, outcome = outcome, treatment = treatment,
        covariates = covariates, n_bins = n_bins, method = "ols", plot = plot
      ))
    }
    fml_gam <- if (!is.null(covariates) && length(covariates) > 0) {
      stats::as.formula(
        paste(outcome, "~ s(", treatment, ") +",
              paste(covariates, collapse = " + "))
      )
    } else {
      stats::as.formula(paste(outcome, "~ s(", treatment, ")"))
    }
    gam_fit   <- mgcv::gam(fml_gam, data = data)
    pred_data <- data.frame(d_seq = bin_mids)
    names(pred_data) <- treatment
    # Add covariate means for marginal prediction
    if (!is.null(covariates) && length(covariates) > 0) {
      for (cv in covariates) pred_data[[cv]] <- mean(data[[cv]], na.rm = TRUE)
    }
    preds <- stats::predict(gam_fit, newdata = pred_data, se.fit = TRUE)
    curve_df <- data.frame(
      dose_mid = bin_mids,
      estimate = as.numeric(preds$fit),
      ci_lo    = as.numeric(preds$fit) - 1.96 * as.numeric(preds$se.fit),
      ci_hi    = as.numeric(preds$fit) + 1.96 * as.numeric(preds$se.fit),
      n        = as.integer(table(bin_idx)[seq_len(n_bins)]),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(curve_df) || nrow(curve_df) == 0) {
    warning("No dose-response estimates produced. Check data and n_bins.")
    return(list(curve_df = curve_df, plot = NULL))
  }

  # ---- Plot ----------------------------------------------------------------
  p <- NULL
  if (plot) {
    method_label <- switch(method,
      ipw = "IPW (Generalised Propensity Score)",
      ols = "Binned OLS",
      gam = "GAM"
    )
    p <- ggplot2::ggplot(curve_df,
           ggplot2::aes(x = dose_mid, y = estimate)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
        fill = "#4575b4", alpha = 0.2, color = NA) +
      ggplot2::geom_line(color = "#4575b4", linewidth = 1.2) +
      ggplot2::geom_point(color = "#4575b4", size = 2.5) +
      ggplot2::labs(
        title    = paste0("Dose-Response Curve (", method_label, ")"),
        subtitle = "Shaded band = 95% CI",
        x        = treatment,
        y        = paste0("E[", outcome, "(d)]")
      ) +
      causalverse::ama_theme()
  }

  list(curve_df = curve_df, plot = p)
}
