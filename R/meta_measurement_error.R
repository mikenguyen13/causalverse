#' Meta-Regression Estimators of Measurement Error from OLS-IV Pairs
#'
#' @description
#' A family of functions that estimate the average proxy quality
#' \eqn{\theta = E[\theta_j]} of mismeasured regressors from a collection of
#' published OLS and IV coefficient pairs, following Pancost and Schaller
#' (2026). Under classical or non-classical measurement error and omitted
#' variable bias, the OLS probability limit is
#' \eqn{\beta_j^{OLS} = \theta_j \beta_j + \gamma_j}, while a valid instrument
#' recovers \eqn{\beta_j}. Regressing \eqn{\hat\beta_j^{OLS}} on
#' \eqn{\hat\beta_j^{IV}} across regressions and correcting the slope for the
#' sampling noise in \eqn{\hat\beta_j^{IV}} identifies \eqn{\theta}.
#'
#' The workflow is:
#' \enumerate{
#'   \item \code{meta_me_prepare()}: map columns, normalize by standard
#'     deviation ratios, winsorize.
#'   \item \code{meta_me_ols()}: meta-OLS estimator (Proposition 2), with
#'     pooled, paper-average, paper fixed-effect, and paper-weighted variants.
#'   \item \code{meta_me_iv()}: meta-IV estimator robust to simultaneity
#'     (Proposition 3), with weak-instrument-robust Anderson-Rubin bounds.
#'   \item Robustness: \code{meta_me_corr_bias()} (Proposition 4),
#'     \code{meta_me_corr_bias_iv()} (Proposition 5),
#'     \code{meta_me_bins()} (weak instruments and \eqn{t}-statistic splits),
#'     \code{meta_me_sorting()} (Proposition 6),
#'     \code{meta_me_invalidity_bounds()} (Proposition 7),
#'     \code{meta_me_heterogeneity()} (Proposition 8 / Corollary 8.1),
#'     \code{meta_me_pubbias()} (structural publication-bias model).
#'   \item Descriptives: \code{meta_me_bias_category()},
#'     \code{meta_me_compare()}, \code{meta_me_nobias_benchmark()},
#'     \code{meta_me_motivation()}.
#' }
#'
#' @references
#' Pancost, N. A., & Schaller, G. (2026). Measuring measurement error.
#' \emph{Review of Finance}. \doi{10.1093/rof/rfag049}
#'
#' Jiang, W. (2017). Have instrumental variables brought us closer to the
#' truth? \emph{Review of Corporate Finance Studies}, 6(2), 127-140.
#'
#' Andrews, I., & Kasy, M. (2019). Identification of and correction for
#' publication bias. \emph{American Economic Review}, 109(8), 2766-2794.
#'
#' @note These functions were written independently from the formulas in
#'   Pancost and Schaller (2026). Their published Stata/Matlab replication
#'   package (Harvard Dataverse, \doi{10.7910/DVN/HOW3J4}) was used only as
#'   a reference to verify that the R implementation reproduces the paper's
#'   numbers; please cite the original article when using these estimators.
#'
#' @name meta_me
NULL

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# Stata-style percentile (summarize, detail / _pctile default) == type 2
.me_pctile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(rep(NA_real_, length(p)))
  unname(stats::quantile(x, p, type = 2, names = FALSE))
}

# Stata-style xtile: category k if cut_{k-1} < x <= cut_k
.me_xtile <- function(x, n) {
  cuts <- .me_pctile(x, seq_len(n - 1) / n)
  out <- rep(NA_integer_, length(x))
  ok <- is.finite(x)
  out[ok] <- findInterval(x[ok], cuts, left.open = TRUE) + 1L
  out
}

.me_winsor <- function(x, lo = 0.01, hi = 0.99, ref = x, lower = TRUE) {
  cut <- .me_pctile(ref, c(lo, hi))
  x[!is.na(x) & x >= cut[2]] <- cut[2]
  if (lower) x[!is.na(x) & x <= cut[1]] <- cut[1]
  x
}

# OLS of y on (1, x) with iid, HC1, or CR1 (Stata small-sample) variance.
.me_lm <- function(y, x, cluster = NULL, type = c("cluster", "hc1", "iid"),
                   intercept = TRUE) {
  type <- match.arg(type)
  X <- if (intercept) cbind(1, x) else cbind(x)
  n <- nrow(X); k <- ncol(X)
  XtXi <- solve(crossprod(X))
  b <- drop(XtXi %*% crossprod(X, y))
  e <- drop(y - X %*% b)
  if (type == "cluster" && !is.null(cluster)) {
    S <- rowsum(X * e, cluster, reorder = FALSE)
    G <- nrow(S)
    meat <- crossprod(S)
    cc <- G / (G - 1) * (n - 1) / (n - k)
  } else if (type == "hc1" || (type == "cluster" && is.null(cluster))) {
    meat <- crossprod(X * e)
    G <- n
    cc <- n / (n - k)
  } else {
    meat <- crossprod(X) * sum(e^2) / (n - k)
    G <- n
    cc <- 1
  }
  V <- cc * XtXi %*% meat %*% XtXi
  sst <- if (intercept) sum((y - mean(y))^2) else sum(y^2)
  r2 <- 1 - sum(e^2) / sst
  r2_adj <- 1 - (1 - r2) * (n - 1) / (n - k)
  list(coef = b, vcov = V, resid = e, n = n, n_clusters = G,
       r2 = r2, r2_adj = r2_adj)
}

# Just-identified 2SLS of y on (1, x) instrumented by (1, z), cluster-robust.
.me_2sls <- function(y, x, z, cluster, small = FALSE) {
  X <- cbind(1, x); Z <- cbind(1, z)
  n <- length(y); k <- 2
  ZXi <- solve(crossprod(Z, X))
  b <- drop(ZXi %*% crossprod(Z, y))
  e <- drop(y - X %*% b)
  S <- rowsum(Z * e, cluster, reorder = FALSE)
  G <- nrow(S)
  cc <- if (small) G / (G - 1) * (n - 1) / (n - k) else 1
  V <- cc * ZXi %*% crossprod(S) %*% t(ZXi)
  list(coef = b, vcov = V, resid = e, n = n, n_clusters = G)
}

# Sample variance / covariance with Stata's n - 1 denominator
.me_var <- function(x) stats::var(x)

.me_lincom_se <- function(V, g) sqrt(drop(t(g) %*% V %*% g))

.me_required <- function(data, cols) {
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    stop("Missing column(s): ", paste(miss, collapse = ", "),
         ". Run meta_me_prepare() first.", call. = FALSE)
  }
}

# ---------------------------------------------------------------------------
# Data preparation
# ---------------------------------------------------------------------------

#' Prepare OLS-IV Pairs for Measurement-Error Meta-Regression
#'
#' Maps user columns to the standard names used by the \code{meta_me_*}
#' functions, optionally normalizes coefficients to be unit-free, and
#' winsorizes. Normalization multiplies \eqn{\hat\beta^{OLS}},
#' \eqn{\hat\beta^{IV}} and their standard errors by
#' \eqn{\hat\sigma_x / \hat\sigma_y}, and the first-stage coefficient and its
#' standard error by \eqn{\hat\sigma_z / \hat\sigma_x}, which recovers the
#' coefficients that would have been reported had \eqn{x} and \eqn{y} been
#' standardized.
#'
#' @param data A data frame with one row per OLS-IV pair.
#' @param paper Column identifying the source paper (cluster).
#' @param b_ols,se_ols,b_iv,se_iv Columns with OLS and IV coefficients and
#'   standard errors.
#' @param b_fs,se_fs,fstat Optional columns with first-stage coefficient,
#'   its standard error, and the first-stage F-statistic.
#' @param sd_x,sd_y,sd_z Optional columns with sample standard deviations of
#'   the regressor, outcome and instrument. \code{sd_x} and \code{sd_y} are
#'   required when \code{normalize = TRUE}; without \code{sd_z} the
#'   first-stage columns are taken as already normalized.
#' @param ols_id Optional column identifying distinct OLS regressions (an OLS
#'   estimate can be paired with several IV estimates). Winsorization cutoffs
#'   for OLS quantities are computed on distinct OLS regressions only.
#' @param fs_id Optional column identifying distinct first-stage regressions,
#'   used the same way for the first-stage cutoff.
#' @param normalize Logical. Normalize by standard-deviation ratios and keep
#'   only normalizable rows. Default \code{TRUE}.
#' @param winsor Numeric. Winsorization level. Coefficients are winsorized in
#'   both tails, standard errors, first-stage coefficients and F-statistics in
#'   the upper tail only. Use \code{0} for none. Default \code{0.01}.
#' @param keep Additional columns to carry through unchanged.
#' @param fs_winsor_ref Optional numeric vector of additional normalized
#'   first-stage coefficients (for example from over-identified
#'   specifications) to pool when computing the first-stage winsorization
#'   cutoff. Pancost and Schaller (2026) pool these.
#' @param fs_cutoff Optional fixed upper winsorization cutoff for the
#'   first-stage coefficient (overrides the computed percentile).
#'
#' @return A data frame with columns \code{paper}, \code{b_ols},
#'   \code{se_ols}, \code{b_iv}, \code{se_iv}, \code{t_iv} and, when
#'   supplied, \code{b_fs}, \code{se_fs}, \code{fstat}, plus \code{keep}
#'   columns. The first-stage coefficient is stored in absolute value.
#'
#' @family meta_me
#' @export
meta_me_prepare <- function(data, paper, b_ols, se_ols, b_iv, se_iv,
                            b_fs = NULL, se_fs = NULL, fstat = NULL,
                            sd_x = NULL, sd_y = NULL, sd_z = NULL,
                            ols_id = NULL, fs_id = NULL, normalize = TRUE,
                            winsor = 0.01, keep = NULL, fs_winsor_ref = NULL,
                            fs_cutoff = NULL) {
  data <- as.data.frame(data)
  g <- function(col) if (is.null(col)) NULL else suppressWarnings(as.numeric(data[[col]]))
  out <- data.frame(paper = data[[paper]],
                    b_ols = g(b_ols), se_ols = g(se_ols),
                    b_iv = g(b_iv), se_iv = g(se_iv))
  out$t_iv <- abs(out$b_iv / out$se_iv)
  if (!is.null(b_fs)) out$b_fs <- abs(g(b_fs))
  if (!is.null(se_fs)) out$se_fs <- g(se_fs)
  if (!is.null(fstat)) out$fstat <- g(fstat)
  if (!is.null(keep)) out <- cbind(out, data[, keep, drop = FALSE])
  ols_key <- if (is.null(ols_id)) {
    paste(out$paper, out$b_ols)
  } else {
    paste(out$paper, data[[ols_id]], out$b_ols)
  }
  fs_ref <- NULL

  if (normalize) {
    if (is.null(sd_x) || is.null(sd_y)) {
      stop("normalize = TRUE requires sd_x and sd_y.", call. = FALSE)
    }
    rxy <- g(sd_x) / g(sd_y)
    for (v in c("b_ols", "se_ols", "b_iv", "se_iv")) out[[v]] <- out[[v]] * rxy
    # without sd_z the first stage is taken as already normalized
    if (!is.null(b_fs) && !is.null(sd_z)) {
      rzx <- g(sd_z) / g(sd_x)
      out$b_fs <- out$b_fs * rzx
      if (!is.null(se_fs)) out$se_fs <- out$se_fs * rzx
    }
    out$t_iv <- abs(out$b_iv / out$se_iv)
  }
  # first-stage cutoff uses distinct first-stage estimates before the
  # normalizable-pair filter, plus any user-supplied extra coefficients
  if (!is.null(out$b_fs)) {
    fs_key <- if (is.null(fs_id)) paste(out$paper, out$b_fs) else paste(out$paper, data[[fs_id]], g(b_fs))
    fs_ref <- out$b_fs[!duplicated(fs_key)]
    if (!is.null(fs_winsor_ref)) fs_ref <- c(fs_ref, abs(fs_winsor_ref))
  }
  ok <- is.finite(out$b_ols) & is.finite(out$b_iv)
  out <- out[ok, , drop = FALSE]
  ols_key <- ols_key[ok]

  if (winsor > 0) {
    first_ols <- !duplicated(ols_key)
    lo <- winsor; hi <- 1 - winsor
    out$b_iv <- .me_winsor(out$b_iv, lo, hi)
    out$se_iv <- .me_winsor(out$se_iv, lo, hi, lower = FALSE)
    out$b_ols <- .me_winsor(out$b_ols, lo, hi, ref = out$b_ols[first_ols])
    out$se_ols <- .me_winsor(out$se_ols, lo, hi, ref = out$se_ols[first_ols],
                             lower = FALSE)
    if (!is.null(out$b_fs)) {
      out$b_fs <- if (is.null(fs_cutoff)) {
        .me_winsor(out$b_fs, lo, hi, ref = fs_ref, lower = FALSE)
      } else {
        pmin(out$b_fs, fs_cutoff)
      }
    }
    if (!is.null(out$se_fs)) out$se_fs <- .me_winsor(out$se_fs, lo, hi, lower = FALSE)
    if (!is.null(out$fstat)) out$fstat <- .me_winsor(out$fstat, lo, hi, lower = FALSE)
  }
  rownames(out) <- NULL
  class(out) <- c("meta_me_data", "data.frame")
  out
}

# ---------------------------------------------------------------------------
# Meta-OLS (Proposition 2)
# ---------------------------------------------------------------------------

#' Meta-OLS Estimator of Average Proxy Quality
#'
#' Regresses \eqn{\hat\beta_j^{OLS}} on \eqn{\hat\beta_j^{IV}} and divides
#' the slope by the reliability ratio
#' \eqn{\hat\rho^2 = 1 - E[(\varsigma_j^{IV})^2] / var(\hat\beta_j^{IV})},
#' which corrects for sampling noise in the IV estimates (Proposition 2 of
#' Pancost and Schaller, 2026). Values of \eqn{\theta} below one indicate
#' attenuation from measurement error.
#'
#' @param data Output of \code{meta_me_prepare()}.
#' @param within How to treat papers reporting many pairs:
#'   \code{"none"} (pooled, every pair weighted equally),
#'   \code{"average"} (one observation per paper, paper means),
#'   \code{"fe"} (paper fixed effects; singleton papers dropped), or
#'   \code{"weighted"} (each paper weighted equally).
#' @param subset Optional logical vector selecting rows of \code{data}.
#'
#' @return An object of class \code{meta_me_ols}: a list with
#'   \code{theta} (implied average proxy quality), \code{theta_se}
#'   (delta-method standard error treating \eqn{\hat\rho^2} as known),
#'   \code{slope}, \code{slope_se}, \code{intercept}, \code{rho2},
#'   \code{ovb} and \code{ovb_se} (implied average omitted-variable bias
#'   \eqn{E[\gamma_j]}), \code{r2}, \code{r2_adj}, \code{n}, \code{n_papers},
#'   \code{moments} (inputs for Propositions 4, 7 and 8) and \code{data}.
#'
#' @details Standard errors are clustered by paper (Stata CR1 small-sample
#'   factor) for \code{"none"}, \code{"fe"} and \code{"weighted"}, and
#'   heteroskedasticity-robust (HC1) for \code{"average"}. The
#'   \code{"weighted"} variant divides both coefficients by
#'   \eqn{\sqrt{J_p}} (\eqn{J_p} pairs in paper \eqn{p}) while keeping an
#'   unweighted intercept, as in the original implementation.
#'
#' @examples
#' set.seed(1)
#' J <- 2000
#' paper <- rep(1:200, each = 10)
#' beta <- rnorm(J, 0.2, 0.5)
#' gamma <- rnorm(J, 0.02, 0.05)
#' se_iv <- rlnorm(J, log(0.15), 0.5)
#' se_ols <- se_iv / 3
#' d <- data.frame(paper = paper,
#'                 b_ols = 0.4 * beta + gamma + rnorm(J, 0, se_ols),
#'                 se_ols = se_ols,
#'                 b_iv = beta + rnorm(J, 0, se_iv), se_iv = se_iv)
#' prep <- meta_me_prepare(d, "paper", "b_ols", "se_ols", "b_iv", "se_iv",
#'                         normalize = FALSE, winsor = 0)
#' meta_me_ols(prep)
#'
#' @family meta_me
#' @export
meta_me_ols <- function(data, within = c("none", "average", "fe", "weighted"),
                        subset = NULL) {
  within <- match.arg(within)
  .me_required(data, c("paper", "b_ols", "se_ols", "b_iv", "se_iv"))
  d <- as.data.frame(data)
  if (!is.null(subset)) d <- d[subset & !is.na(subset), , drop = FALSE]
  d <- d[is.finite(d$b_ols) & is.finite(d$b_iv) & is.finite(d$se_iv), , drop = FALSE]
  Jp <- stats::ave(d$se_iv, d$paper, FUN = length)

  if (within == "none") {
    fit <- .me_lm(d$b_ols, d$b_iv, d$paper)
    xvar <- .me_var(d$b_iv)
    Ez <- .me_var(d$se_iv) + mean(d$se_iv)^2
    xbar <- mean(d$b_iv)
  } else if (within == "average") {
    avg_iv <- tapply(d$b_iv, d$paper, mean)
    avg_ols <- tapply(d$b_ols, d$paper, mean)
    zeta <- tapply(d$se_iv^2, d$paper, function(s) sum(s) / length(s)^2)
    fit <- .me_lm(as.numeric(avg_ols), as.numeric(avg_iv), type = "hc1")
    xvar <- .me_var(avg_iv)
    Ez <- mean(zeta)
    xbar <- mean(avg_iv)
  } else if (within == "fe") {
    d <- d[Jp > 1, , drop = FALSE]
    Jp <- Jp[Jp > 1]
    dm <- function(v) v - stats::ave(v, d$paper)
    # absorbed FE nested in clusters do not count against df (as in reghdfe)
    fit <- .me_lm(dm(d$b_ols), dm(d$b_iv), d$paper, intercept = FALSE)
    xvar <- .me_var(dm(d$b_iv))
    Ez <- mean((1 - 1 / Jp) * d$se_iv^2)
    xbar <- mean(d$b_iv)
  } else {
    w <- 1 / sqrt(Jp)
    fit <- .me_lm(d$b_ols * w, d$b_iv * w, d$paper)
    EJinv <- mean(1 / Jp)
    EJinvb <- mean(d$b_iv / Jp)
    xvar <- mean((d$b_iv - EJinvb / EJinv)^2 / Jp)
    Ez <- mean(d$se_iv^2 / Jp)
    xbar <- mean(d$b_iv * w)
  }

  slope_pos <- if (within == "fe") 1 else 2
  b <- fit$coef[slope_pos]
  b_se <- sqrt(fit$vcov[slope_pos, slope_pos])
  rho2 <- 1 - Ez / xvar
  theta <- b / rho2
  theta_se <- b_se / abs(rho2)

  ovb <- ovb_se <- NA_real_
  if (within != "fe") {
    kxbar <- (1 - rho2) * xbar / rho2
    ovb <- fit$coef[1] - kxbar * b
    ovb_se <- .me_lincom_se(fit$vcov, c(1, -kxbar))
  }

  moments <- NULL
  if (within == "none") {
    var_ols <- .me_var(d$b_ols)
    Ez_ols <- .me_var(d$se_ols) + mean(d$se_ols)^2
    phat2 <- 1 - Ez_ols / var_ols
    R2 <- fit$r2_adj
    w1 <- (1 - R2 / (rho2 * phat2)) * phat2 / rho2 * var_ols / xvar
    iota <- 1 + mean(d$b_iv)^2 / (rho2 * xvar)
    moments <- list(var_iv = xvar, var_ols = var_ols, Ezeta2_iv = Ez,
                    Ezeta2_ols = Ez_ols, mean_iv = mean(d$b_iv),
                    mean_ols = mean(d$b_ols),
                    cov_iv_ols = stats::cov(d$b_iv, d$b_ols),
                    rho2 = rho2, phat2 = phat2, w1 = w1, iota = iota,
                    B = theta, max_var_theta = w1 / iota,
                    rho2_mean_sq = 1 - mean(d$se_iv^2) / xvar,
                    phat2_mean_sq = 1 - mean(d$se_ols^2) / var_ols)
  }

  structure(list(theta = unname(theta), theta_se = unname(theta_se),
                 slope = unname(b), slope_se = unname(b_se),
                 intercept = if (within == "fe") NA_real_ else unname(fit$coef[1]),
                 rho2 = rho2, var_iv = xvar, Ezeta2_iv = Ez,
                 ovb = unname(ovb), ovb_se = unname(ovb_se),
                 r2 = fit$r2, r2_adj = fit$r2_adj,
                 n = fit$n, n_papers = length(unique(d$paper)),
                 within = within, moments = moments,
                 t_iv_median = stats::median(d$t_iv),
                 t_iv_min = min(d$t_iv),
                 fstat_median = if (!is.null(d$fstat)) stats::median(d$fstat, na.rm = TRUE) else NA_real_,
                 fstat_min = if (!is.null(d$fstat) && any(is.finite(d$fstat))) min(d$fstat, na.rm = TRUE) else NA_real_,
                 data = d),
            class = "meta_me_ols")
}

#' @export
print.meta_me_ols <- function(x, ...) {
  cat("Meta-OLS measurement-error estimator (Pancost & Schaller 2026)\n")
  cat(sprintf("  within-paper treatment: %s\n", x$within))
  cat(sprintf("  implied theta = %.3f (SE %.3f)\n", x$theta, x$theta_se))
  cat(sprintf("  meta slope = %.3f (SE %.3f), rho^2 = %.3f\n",
              x$slope, x$slope_se, x$rho2))
  if (is.finite(x$ovb)) cat(sprintf("  implied E[OVB] = %.4f (SE %.4f)\n", x$ovb, x$ovb_se))
  cat(sprintf("  pairs = %d, papers = %d\n", x$n, x$n_papers))
  invisible(x)
}

# ---------------------------------------------------------------------------
# Meta-IV (Proposition 3)
# ---------------------------------------------------------------------------

#' Meta-IV Estimator of Average Proxy Quality
#'
#' Robust to simultaneity bias in the underlying OLS regressions. Multiplies
#' both coefficients by the first-stage coefficient and estimates
#' \eqn{\hat\beta^{OLS}_j \hat\pi_j = a + b\, \hat\beta^{IV}_j \hat\pi_j}
#' by 2SLS using \eqn{\hat\beta^{IV}_j} as the instrument (Proposition 3 of
#' Pancost and Schaller, 2026). With several instruments, \code{b_fs} can be
#' any linear combination of the first-stage coefficients (Corollary 3.1),
#' for example a precision-weighted average or the first one reported.
#'
#' @inheritParams meta_me_ols
#' @param small Logical. Apply the CR1 small-sample factor to the 2SLS
#'   variance. Default \code{FALSE} (large-sample, as in \code{ivreg2}).
#' @param level Confidence level for the Anderson-Rubin set. Default 0.95.
#'
#' @return An object of class \code{meta_me_iv} with \code{theta},
#'   \code{theta_se}, the weak-instrument-robust Anderson-Rubin interval for
#'   \eqn{\theta} (\code{theta_ar}), the meta first-stage coefficient and
#'   cluster-robust F-statistic, the OLS of the transformed regression,
#'   \code{rho2}, \code{ovb}, sample sizes and \code{moments} (inputs to
#'   \code{meta_me_corr_bias_iv()}).
#'
#' @family meta_me
#' @export
meta_me_iv <- function(data, subset = NULL, small = FALSE, level = 0.95) {
  .me_required(data, c("paper", "b_ols", "se_ols", "b_iv", "se_iv", "b_fs"))
  d <- as.data.frame(data)
  if (!is.null(subset)) d <- d[subset & !is.na(subset), , drop = FALSE]
  d <- d[is.finite(d$b_ols) & is.finite(d$b_iv) & is.finite(d$b_fs) &
           is.finite(d$se_iv), , drop = FALSE]
  if (nrow(d) < 5) stop("Too few pairs with first-stage coefficients.", call. = FALSE)
  yy <- d$b_ols * d$b_fs
  xx <- d$b_iv * d$b_fs
  zz <- d$b_iv
  cl <- d$paper

  ols_fit <- .me_lm(yy, xx, cl)
  fs_fit <- .me_lm(xx, zz, cl)
  iv_fit <- .me_2sls(yy, xx, zz, cl, small = small)
  # cluster-robust first-stage F (Kleibergen-Paap with one instrument)
  fs_large <- .me_2sls(xx, zz, zz, cl, small = small)
  fstat <- fs_large$coef[2]^2 / fs_large$vcov[2, 2]

  num <- mean(d$b_fs * d$se_iv^2)
  den <- mean(d$b_fs * d$b_iv^2) - mean(d$b_fs * d$b_iv) * mean(d$b_iv)
  rho2 <- 1 - num / den
  b <- iv_fit$coef[2]
  b_se <- sqrt(iv_fit$vcov[2, 2])

  kxbar <- (1 - rho2) * mean(xx) / rho2
  ovb <- iv_fit$coef[1] - kxbar * b
  ovb_se <- .me_lincom_se(iv_fit$vcov, c(1, -kxbar))

  ar <- .me_ar_ci(yy, xx, zz, cl, level = level, small = small)

  moments <- NULL
  if ("se_fs" %in% names(d) && any(is.finite(d$se_fs))) {
    moments <- .me_iv_moments(d)
  }

  structure(list(theta = unname(b / rho2), theta_se = unname(b_se / abs(rho2)),
                 theta_ar = ar / rho2, slope = unname(b), slope_se = unname(b_se),
                 rho2 = rho2, ovb = unname(ovb), ovb_se = unname(ovb_se),
                 first_stage = unname(fs_fit$coef[2]),
                 first_stage_se = sqrt(fs_fit$vcov[2, 2]),
                 first_stage_F = unname(fstat),
                 ols_slope = unname(ols_fit$coef[2]),
                 ols_slope_se = sqrt(ols_fit$vcov[2, 2]),
                 n = length(yy), n_papers = length(unique(cl)),
                 t_iv_median = stats::median(d$t_iv), t_iv_min = min(d$t_iv),
                 fstat_median = if (!is.null(d$fstat)) stats::median(d$fstat, na.rm = TRUE) else NA_real_,
                 fstat_min = if (!is.null(d$fstat) && any(is.finite(d$fstat))) min(d$fstat, na.rm = TRUE) else NA_real_,
                 moments = moments, data = d),
            class = "meta_me_iv")
}

#' @export
print.meta_me_iv <- function(x, ...) {
  cat("Meta-IV measurement-error estimator (Pancost & Schaller 2026)\n")
  cat(sprintf("  implied theta = %.3f (SE %.3f), AR set [%.2f, %.2f]\n",
              x$theta, x$theta_se, x$theta_ar[1], x$theta_ar[2]))
  cat(sprintf("  meta first stage = %.3f (SE %.3f), F = %.1f\n",
              x$first_stage, x$first_stage_se, x$first_stage_F))
  cat(sprintf("  pairs = %d, papers = %d\n", x$n, x$n_papers))
  invisible(x)
}

# Anderson-Rubin set for b in y = a + b x, instrument z (just identified),
# cluster-robust score form with the variance evaluated under the null (as in
# Stata's weakiv). The statistic is a ratio of quadratics in b0, so the set
# has a closed form. Returns c(lower, upper); Inf when unbounded.
.me_ar_ci <- function(y, x, z, cluster, level = 0.95, small = FALSE) {
  n <- length(y)
  zt <- z - mean(z); yt <- y - mean(y); xt <- x - mean(x)
  Ay <- sum(zt * yt); Ax <- sum(zt * xt)
  sy <- rowsum(zt * yt, cluster, reorder = FALSE)[, 1]
  sx <- rowsum(zt * xt, cluster, reorder = FALSE)[, 1]
  G <- length(sy)
  cc <- if (small) G / (G - 1) * (n - 1) / (n - 2) else 1
  crit <- stats::qchisq(level, 1) * cc
  A <- Ax^2 - crit * sum(sx^2)
  B <- -2 * (Ay * Ax - crit * sum(sx * sy))
  C <- Ay^2 - crit * sum(sy^2)
  disc <- B^2 - 4 * A * C
  if (A > 0 && disc > 0) {
    r <- sort((-B + c(-1, 1) * sqrt(disc)) / (2 * A))
    return(r)
  }
  c(-Inf, Inf)
}

# Moments for Proposition 5 (meta-IV robustness to Assumption 4(i)). Terms
# that involve the first-stage standard error use the pairs that report it;
# all other moments use the full meta-IV sample. Products of noisy terms are
# winsorized at 1/99 percent.
.me_iv_moments <- function(d) {
  w <- function(v) .me_winsor(v, ref = v[is.finite(v)])
  mw <- function(v) mean(w(v), na.rm = TRUE)
  fs <- d$b_fs; zf <- d$se_fs
  bo <- d$b_ols; zo <- d$se_ols; bi <- d$b_iv; zi <- d$se_iv
  of <- bo * fs
  mu_of <- mean(of)
  var_of <- .me_var(of)
  s2_of <- var_of + mw(zo^2 * zf^2) - mw(bo^2 * zf^2) - mw(fs^2 * zo^2)
  var_if <- .me_var(w(bi * fs))
  s2_if <- var_if + mw(zi^2 * zf^2) - mw(bi^2 * zf^2) - mw(fs^2 * zi^2)
  var_i <- .me_var(bi)
  s2_i <- var_i - mean(zi^2)
  cov_ofif <- stats::cov(w(of), w(bi * fs)) - mw(bo * bi * zf^2)
  corr_ofif <- cov_ofif / (sqrt(var_of) * sqrt(var_if))
  corr_ofi <- stats::cov(w(of), bi) / (sqrt(var_i) * sqrt(var_of))
  corr_ifi <- (stats::cov(w(bi * fs), bi) - mw(fs * zi^2)) /
    (sqrt(var_i) * sqrt(var_if))
  list(mu_of = mu_of, sigma2_of = s2_of, sigma2_if = s2_if, sigma2_i = s2_i,
       corr_ofif = corr_ofif, corr_ofi = corr_ofi, corr_ifi = corr_ifi,
       b_iv = corr_ofi * sqrt(s2_of) / (corr_ifi * sqrt(s2_if)))
}

# ---------------------------------------------------------------------------
# Proposition 4: correlation between causal effects and OVB (meta-OLS)
# ---------------------------------------------------------------------------

#' Bias-Adjusted Proxy Quality Allowing Correlated Effects and OVB
#'
#' Relaxes the assumption that true effects \eqn{\beta_j} and omitted
#' variable bias \eqn{\gamma_j} are uncorrelated (Proposition 4). Returns the
#' implied \eqn{\theta} over a grid of correlations \eqn{R} and variances of
#' \eqn{\theta_j}. Positive \eqn{R} implies more measurement error than the
#' baseline estimate.
#'
#' @param fit A \code{meta_me_ols} object fit with \code{within = "none"}.
#' @param R Numeric vector of correlations \eqn{corr(\beta_j, \gamma_j)}.
#' @param var_theta Numeric vector of values for \eqn{var(\theta_j)}; values
#'   above \code{fit$moments$max_var_theta} are infeasible.
#'
#' @return A data frame with columns \code{R}, \code{var_theta}, \code{theta}.
#' @family meta_me
#' @export
meta_me_corr_bias <- function(fit, R = c(0.3, 0, -0.3), var_theta = 0) {
  m <- fit$moments
  if (is.null(m)) stop("fit must use within = 'none'.", call. = FALSE)
  g <- expand.grid(R = R, var_theta = var_theta)
  inside <- g$R^2 / (1 - g$R^2) * (m$w1 - g$var_theta * m$iota)
  adj <- ifelse(inside >= 0, sqrt(pmax(inside, 0)), NA_real_)
  g$theta <- m$B - sign(g$R) * adj
  g$theta[g$R == 0] <- m$B
  g
}

# ---------------------------------------------------------------------------
# Proposition 5: meta-IV analogue (quartic)
# ---------------------------------------------------------------------------

.me_quartic_iv <- function(rsqr, vartau, m) {
  a <- -(1 - m$corr_ifi^2 / rsqr) * m$sigma2_if
  b <- 2 * (m$corr_ofif - m$corr_ifi * m$corr_ofi / rsqr) * sqrt(m$sigma2_of * m$sigma2_if)
  c <- -((1 - m$corr_ofi^2 / rsqr) * m$sigma2_of + (1 - m$corr_ifi^2 / rsqr) * m$sigma2_if * vartau)
  dd <- 2 * (m$corr_ofif - m$corr_ifi * m$corr_ofi / rsqr) * sqrt(m$sigma2_of * m$sigma2_if) * vartau
  e <- (m$corr_ofi^2 / rsqr + m$mu_of^2 / m$sigma2_of) * m$sigma2_of * vartau
  r <- polyroot(c(e, dd, c, b, a))
  r <- Re(r[abs(Im(r)) < 1e-8])
  r <- r[r >= 0 & r <= 1 & !(vartau > r * (1 - r))]
  r <- sort(r)
  c(r[1], r[2])
}

#' Meta-IV Proxy Quality Allowing Correlated Residual Parameters
#'
#' Relaxes Assumption 4(i) of the meta-IV estimator (Proposition 5). The
#' implied \eqn{\theta} solves a quartic; the smaller valid root corresponds
#' to \eqn{R > 0} and the larger to \eqn{R < 0}.
#'
#' @param fit A \code{meta_me_iv} object whose data included \code{se_fs}.
#' @param R Correlation values.
#' @param var_theta Values for \eqn{var(\theta_j)}.
#'
#' @return A data frame with \code{R}, \code{var_theta}, \code{theta}, and
#'   attribute \code{max_var_theta}.
#' @family meta_me
#' @export
meta_me_corr_bias_iv <- function(fit, R = c(0.3, -0.3), var_theta = c(1e-8, 0.03)) {
  m <- fit$moments
  if (is.null(m)) stop("fit needs first-stage standard errors (se_fs).", call. = FALSE)
  g <- expand.grid(R = R, var_theta = var_theta)
  g$theta <- mapply(function(r, v) {
    rr <- .me_quartic_iv(r^2, v, m)
    if (r > 0) rr[1] else rr[2]
  }, g$R, g$var_theta)
  # largest var(theta) at which a perfect correlation still rationalizes the
  # meta-IV slope: smallest root of the R^2 = 1 quartic equals b_IV
  f <- function(v) .me_quartic_iv(1, v, m)[1] - m$b_iv
  vg <- seq(1e-8, 0.25, length.out = 2501)
  fv <- vapply(vg, f, 0)
  k <- which(is.finite(fv[-1]) & is.finite(fv[-length(fv)]) & sign(fv[-1]) != sign(fv[-length(fv)]))
  vmax <- if (length(k)) stats::uniroot(f, vg[c(k[1], k[1] + 1)])$root else NA_real_
  attr(g, "max_var_theta") <- vmax
  g
}

# ---------------------------------------------------------------------------
# Proposition 6: cross-study sorting by |beta_IV|
# ---------------------------------------------------------------------------

#' Proxy Quality by Magnitude of the IV Estimate
#'
#' Splits the sample into quantile bins of \eqn{|\hat\beta^{IV}_j|} and
#' applies the conditional noise correction of Proposition 6, which allows
#' proxy quality to vary with the size of the causal effect.
#'
#' @inheritParams meta_me_ols
#' @param n_bins Number of quantile bins. Default 5.
#' @param mu,sigma2 Mean and variance of true effects \eqn{\beta_j}. Default
#'   \code{NULL} uses \eqn{E[\hat\beta^{IV}]} and
#'   \eqn{var(\hat\beta^{IV}) - E[\varsigma^2]} from the full sample.
#'
#' @return A data frame, one row per bin, with the conditional
#'   \eqn{\hat\rho^2_k}, \eqn{\theta_k} and its standard error, the
#'   unconditional-correction estimate, and bin descriptors.
#' @family meta_me
#' @export
meta_me_sorting <- function(data, n_bins = 5, mu = NULL, sigma2 = NULL) {
  d <- as.data.frame(data)
  if (is.null(mu)) mu <- mean(d$b_iv)
  if (is.null(sigma2)) sigma2 <- .me_var(d$b_iv) - mean(d$se_iv^2)
  bin <- .me_xtile(abs(d$b_iv), n_bins)
  do.call(rbind, lapply(seq_len(n_bins), function(k) {
    s <- d[bin == k, , drop = FALSE]
    fit <- .me_lm(s$b_ols, s$b_iv, s$paper)
    b <- fit$coef[2]; bse <- sqrt(fit$vcov[2, 2])
    rhoj <- sigma2 / (sigma2 + s$se_iv^2)
    num <- mean(s$b_iv^2 * rhoj) - mean(s$b_iv * rhoj) * (mu + mean(s$b_iv)) +
      mu * mean(s$b_iv) * mean(rhoj)
    rho2k <- num / .me_var(s$b_iv)
    vb <- .me_var(s$b_iv); Ez <- .me_var(s$se_iv) + mean(s$se_iv)^2
    data.frame(bin = k, theta = unname(b / rho2k), theta_se = unname(bse / abs(rho2k)),
               rho2 = rho2k, theta_uncond = unname(b * abs(vb / (vb - Ez))),
               min_abs_iv = min(abs(s$b_iv)), median_abs_iv = stats::median(abs(s$b_iv)),
               n = nrow(s), n_papers = length(unique(s$paper)))
  }))
}

# ---------------------------------------------------------------------------
# Proposition 7: instrument invalidity bounds
# ---------------------------------------------------------------------------

#' Bounds on Proxy Quality under Instrument Invalidity
#'
#' Instrument invalidity acts like measurement error in
#' \eqn{\hat\beta^{IV}_j}. The meta-OLS estimate is a lower bound and the
#' inverse noise-adjusted reverse regression an upper bound (Proposition 7):
#' \deqn{\hat b/\hat\rho^2 \le \theta \le \hat p^2 var(\hat\beta^{OLS}) /
#' (\hat b\, var(\hat\beta^{IV})).}
#'
#' @param fit A \code{meta_me_ols} object fit with \code{within = "none"}.
#' @return A named numeric vector with \code{lower}, \code{upper},
#'   \code{rho2}, \code{phat2}, \code{var_iv}, \code{var_ols}.
#' @family meta_me
#' @export
meta_me_invalidity_bounds <- function(fit) {
  m <- fit$moments
  if (is.null(m)) stop("fit must use within = 'none'.", call. = FALSE)
  c(lower = fit$theta, upper = m$phat2 * m$var_ols / (m$var_iv * fit$slope),
    rho2 = m$rho2, phat2 = m$phat2, var_iv = m$var_iv, var_ols = m$var_ols)
}

# ---------------------------------------------------------------------------
# Proposition 8 / Corollary 8.1: heterogeneous treatment effects
# ---------------------------------------------------------------------------

#' Proxy Quality under Heterogeneous Treatment Effects
#'
#' With heterogeneous effects, OLS and IV weight individual effects
#' differently; their gap \eqn{H_j = \beta^x_j - \beta^z_j} biases the
#' meta-OLS estimator when it varies across regressions and correlates with
#' the LATE (Proposition 8). Solves the quartic of Corollary 8.1 for
#' \eqn{\theta} over grids of \eqn{R = corr(\beta^z_j, H_j)} and
#' \eqn{E[H_j]}.
#'
#' @param fit A \code{meta_me_ols} object fit with \code{within = "none"}.
#' @param R Numeric vector of correlations.
#' @param var_theta Values of \eqn{var(\theta_j)}.
#' @param var_gamma Variance of OVB. Default 0 (largest bias).
#' @param mu_H Values of \eqn{E[H_j]}. Default 0.
#' @param replicate_original Logical. The original Matlab code
#'   (\code{solve_hte.m}) sets \eqn{E[\beta]} equal to
#'   \eqn{cov(\hat\beta^{IV}, \hat\beta^{OLS})} because of an indexing slip.
#'   \code{TRUE} reproduces the published Figure 8; \code{FALSE} (default)
#'   uses the sample mean of \eqn{\hat\beta^{IV}}.
#'
#' @return A data frame with \code{R}, \code{var_theta}, \code{mu_H},
#'   \code{theta} (\code{NA} when no unique admissible root exists).
#' @family meta_me
#' @export
meta_me_heterogeneity <- function(fit, R = seq(-0.99, 0.99, length.out = 199),
                                  var_theta = c(1e-8, 0.04), var_gamma = 0,
                                  mu_H = 0, replicate_original = FALSE) {
  m <- fit$moments
  if (is.null(m)) stop("fit must use within = 'none'.", call. = FALSE)
  rho2 <- m$rho2_mean_sq; phat <- m$phat2_mean_sq
  vi <- m$var_iv; vo <- m$var_ols; cc <- m$cov_iv_ols
  mu_beta <- if (replicate_original) cc else m$mean_iv
  tstar <- cc / (vi * rho2)
  g <- expand.grid(R = R, var_theta = var_theta, mu_H = mu_H)
  g$theta <- mapply(function(r, vt, mh) {
    if (r == 0) r <- 1e-6
    a4 <- rho2 * vi * (1 - r^2)
    a3 <- -2 * cc * (1 - r^2)
    a0 <- cc^2 / (rho2 * vi) * vt
    a2 <- a0 / vt + a4 * vt + r^2 * ((mu_beta + mh)^2 * vt + var_gamma - phat * vo)
    a1 <- a3 * vt
    rt <- polyroot(c(a0, a1, a2, a3, a4))
    rt <- Re(rt[abs(Im(rt)) < 1e-6])
    rt <- if (r > 0) rt[rt <= tstar] else rt[rt >= tstar]
    if (length(rt) == 1) rt else NA_real_
  }, g$R, g$var_theta, g$mu_H)
  g
}

# ---------------------------------------------------------------------------
# Subsample splits (weak instruments, publication bias)
# ---------------------------------------------------------------------------

#' Proxy Quality across Bins of the First-Stage F or IV t-Statistic
#'
#' One bin below a threshold (F < 10 or |t| < 1.96) and \code{n_bins}
#' equal-sized bins above it, as in the weak-instrument and publication-bias
#' checks of Pancost and Schaller (2026). If instrument weakness or selective
#' publication drove the results, \eqn{\theta} would trend with the bins.
#'
#' @inheritParams meta_me_ols
#' @param by \code{"fstat"} or \code{"t_iv"}.
#' @param threshold Threshold value. Defaults to 10 for F and 1.96 for t.
#' @param n_bins Number of bins above the threshold.
#' @param estimator \code{"ols"} (meta-OLS) or \code{"iv"} (meta-IV).
#' @param subset Optional logical row selector applied first (for example
#'   the main result per paper).
#' @param missing_in_first_bin Logical. The original code assigns pairs with
#'   a missing binning variable (unreported F) to the below-threshold bin.
#'   \code{TRUE} reproduces the published Table 9; \code{FALSE} (default)
#'   drops them.
#'
#' @return A data frame, one row per bin, with \code{theta}, \code{theta_se},
#'   \code{rho2}, min and median of the binning variable, \code{n},
#'   \code{n_papers}.
#' @family meta_me
#' @export
meta_me_bins <- function(data, by = c("fstat", "t_iv"), threshold = NULL,
                         n_bins = 3, estimator = c("ols", "iv"), subset = NULL,
                         missing_in_first_bin = FALSE) {
  by <- match.arg(by); estimator <- match.arg(estimator)
  d <- as.data.frame(data)
  if (!is.null(subset)) d <- d[subset & !is.na(subset), , drop = FALSE]
  if (estimator == "iv") d <- d[is.finite(d$b_fs), , drop = FALSE]
  if (is.null(threshold)) threshold <- if (by == "fstat") 10 else 1.96
  v <- d[[by]]
  if (!missing_in_first_bin) {
    d <- d[is.finite(v), , drop = FALSE]; v <- v[is.finite(v)]
  }
  bin <- rep(1L, length(v))
  above <- is.finite(v) & v >= threshold
  if (n_bins > 1) bin[above] <- 1L + .me_xtile(v[above], n_bins) else bin[above] <- 2L
  do.call(rbind, lapply(sort(unique(bin)), function(k) {
    s <- bin == k
    if (estimator == "ols") {
      f <- meta_me_ols(d, subset = s)
      vb <- f$var_iv; Ez <- f$Ezeta2_iv
      th <- f$slope * abs(vb / (vb - Ez)); thse <- f$slope_se * abs(vb / (vb - Ez))
    } else {
      f <- meta_me_iv(d, subset = s)
      th <- f$theta; thse <- f$theta_se
    }
    data.frame(bin = k, theta = th, theta_se = thse, rho2 = f$rho2,
               min = min(v[s], na.rm = TRUE),
               median = stats::median(v[s], na.rm = TRUE),
               n = f$n, n_papers = f$n_papers)
  }))
}

# ---------------------------------------------------------------------------
# Descriptives: expected bias direction, IV vs OLS comparisons
# ---------------------------------------------------------------------------

#' Classify OLS-IV Pairs by Expected Direction of OLS Bias
#'
#' Combines the authors' stated sign of simultaneity bias, omitted variable
#' bias and measurement-error bias into an expected bias direction and labels
#' each pair \code{"Affirmative"} (theory predicts \eqn{|IV| < |OLS|}),
#' \code{"Corrective"} (theory predicts \eqn{|IV| > |OLS|}) or
#' \code{"Unclear"}, following Jiang (2017).
#'
#' @param sim_sign,ovb_sign,me_sign Numeric vectors coded +1 (upward OLS
#'   bias), -1 (downward), 0 (not stated or ambiguous).
#' @param b_iv IV coefficients.
#' @param replicate_original Logical. In the original Stata code a combined
#'   direction of 0 (no direction stated) satisfies
#'   \code{sign(0) != sign(b_iv)} and is therefore labelled "Corrective",
#'   and only missing codes are "Unclear". \code{TRUE} reproduces the
#'   published Table 4 and Figure 3; \code{FALSE} (default) labels pairs
#'   without a stated direction "Unclear".
#' @return A character vector of categories.
#' @family meta_me
#' @export
meta_me_bias_category <- function(sim_sign, ovb_sign, me_sign, b_iv,
                                  replicate_original = FALSE) {
  if (replicate_original) {
    # Stata semantics: missing compares equal to missing, != 0 is TRUE for missing
    eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    neq0 <- function(a) is.na(a) | a != 0
    dir <- ifelse(eq(ovb_sign, 0) | eq(sim_sign, ovb_sign), sim_sign, NA)
    s0 <- eq(sim_sign, 0); dir[s0] <- ovb_sign[s0]
    dir[neq0(ovb_sign) & neq0(sim_sign) & !eq(ovb_sign, sim_sign)] <- 0
    dir[eq(ovb_sign, 0) & eq(sim_sign, 0)] <- 0
    use_me <- eq(dir, 0) & neq0(me_sign)
    dir[use_me] <- me_sign[use_me]
    out <- rep("Unclear", length(dir))
    nm <- !is.na(dir)
    out[nm & sign(dir) == sign(b_iv)] <- "Affirmative"
    out[nm & sign(dir) != sign(b_iv)] <- "Corrective"
    return(out)
  }
  sim_sign[is.na(sim_sign)] <- 0; ovb_sign[is.na(ovb_sign)] <- 0
  me_sign[is.na(me_sign)] <- 0
  dir <- ifelse(ovb_sign == 0 | sim_sign == ovb_sign, sim_sign, NA)
  dir[sim_sign == 0] <- ovb_sign[sim_sign == 0]
  dir[ovb_sign != 0 & sim_sign != 0 & ovb_sign != sim_sign] <- 0
  dir[ovb_sign == 0 & sim_sign == 0] <- 0
  use_me <- dir == 0 & me_sign != 0
  dir[use_me] <- me_sign[use_me]
  out <- rep("Unclear", length(dir))
  nz <- dir != 0
  out[nz & sign(dir) == sign(b_iv)] <- "Affirmative"
  out[nz & sign(dir) != sign(b_iv)] <- "Corrective"
  out
}

#' Compare IV and OLS Magnitudes
#'
#' Share of same-sign pairs where \eqn{|\hat\beta^{IV}| > |\hat\beta^{OLS}|},
#' share that switch sign, and the mean and median of
#' \eqn{|\hat\beta^{IV}/\hat\beta^{OLS}|}, optionally by group.
#'
#' @param b_ols,b_iv Numeric vectors (normalization is irrelevant here).
#' @param group Optional grouping vector (for example the output of
#'   \code{meta_me_bias_category()} or significance of the IV estimate).
#' @return A data frame with one row per group plus \code{"All"}.
#' @family meta_me
#' @export
meta_me_compare <- function(b_ols, b_iv, group = NULL) {
  if (is.null(group)) group <- rep("All", length(b_iv))
  f <- function(o, i) {
    same <- sign(i) == sign(o)
    ratio <- abs(i / o); ratio <- ratio[is.finite(ratio)]
    data.frame(n = length(i),
               pct_iv_bigger_same_sign = 100 * mean(abs(i[same]) > abs(o[same])),
               pct_iv_bigger_all = 100 * mean(abs(i) > abs(o)),
               pct_switch_sign = 100 * mean(!same),
               mean_ratio = mean(ratio), median_ratio = stats::median(ratio))
  }
  groups <- unique(stats::na.omit(group))
  rows <- lapply(groups, function(gg) cbind(group = gg, f(b_ols[group %in% gg], b_iv[group %in% gg])))
  if (length(groups) > 1) rows <- c(rows, list(cbind(group = "All", f(b_ols, b_iv))))
  do.call(rbind, rows)
}

#' Counterfactual IV-OLS Comparisons Absent Any Bias
#'
#' Because IV standard errors exceed OLS standard errors, \eqn{|IV| > |OLS|}
#' occurs more than half the time even without bias. This function
#' deconvolves the distribution of true effects from \eqn{\hat\beta^{IV}}
#' by nonparametric maximum likelihood (EM on a grid, within cells defined by
#' quintiles of the OLS and IV standard errors) and computes the
#' no-bias probability \eqn{Pr(|OLS| < |IV| \mid \text{same sign})}.
#'
#' @param b_iv IV coefficients.
#' @param se_ols,se_iv Standard errors.
#' @param n_grid Grid points. Default 151.
#' @param n_bins Quantile bins per standard error. Default 5.
#' @return Named numeric: \code{prob} (counterfactual percentage), and the
#'   numerator and denominator averages.
#' @family meta_me
#' @export
meta_me_nobias_benchmark <- function(b_iv, se_ols, se_iv, n_grid = 151, n_bins = 5) {
  ok <- is.finite(b_iv) & is.finite(se_ols) & is.finite(se_iv)
  x <- b_iv[ok]; p <- abs(se_ols[ok]); q <- abs(se_iv[ok])
  N <- length(x)
  NUM <- 0; DEN <- 0
  zero_q <- q == 0
  if (any(zero_q)) {
    NUM <- NUM + sum(.me_Aprob(x[zero_q], p[zero_q], 0))
    DEN <- DEN + sum(.me_Dprob(x[zero_q], p[zero_q], 0))
  }
  pos <- !zero_q
  pc <- rep(NA_integer_, N); qc <- rep(NA_integer_, N)
  pc[pos & p == 0] <- 0L
  sel <- pos & p > 0
  pc[sel] <- .me_xtile(p[sel], n_bins)
  qc[pos] <- .me_xtile(q[pos], n_bins)
  cell <- ifelse(pos, paste(pc, qc), NA)
  for (cc in unique(stats::na.omit(cell))) {
    i <- which(cell == cc)
    xc <- x[i]; pc_ <- p[i]; qc_ <- q[i]
    zlo <- min(xc - 4 * qc_); zhi <- max(xc + 4 * qc_)
    if (zhi <= zlo) zhi <- zlo + 1e-6
    zg <- seq(zlo, zhi, length.out = n_grid)
    L <- stats::dnorm(outer(xc, zg, "-") / qc_) / qc_
    mix <- rep(1 / n_grid, n_grid)
    for (it in seq_len(5000)) {
      Rm <- sweep(L, 2, mix, "*")
      rs <- pmax(rowSums(Rm), 1e-300)
      mixnew <- colMeans(Rm / rs)
      if (max(abs(mixnew - mix)) < 1e-9) { mix <- mixnew; break }
      mix <- mixnew
    }
    A <- vapply(seq_along(i), function(k) sum(mix * .me_Aprob(zg, pc_[k], qc_[k])), 0)
    D <- vapply(seq_along(i), function(k) sum(mix * .me_Dprob(zg, pc_[k], qc_[k])), 0)
    # cells share (p, q) only approximately, so evaluate per observation
    NUM <- NUM + sum(A); DEN <- DEN + sum(D)
  }
  c(prob = 100 * NUM / DEN, num = NUM / N, den = DEN / N, n = N)
}

# Pr(same sign) for OLS ~ N(z, p^2), IV ~ N(z, q^2) independent (vectorized)
.me_Dprob <- function(z, p, q) {
  n <- max(length(z), length(p), length(q))
  z <- rep_len(z, n); p <- rep_len(p, n); q <- rep_len(q, n)
  out <- stats::pnorm(z / p) * stats::pnorm(z / q) + stats::pnorm(-z / p) * stats::pnorm(-z / q)
  q0 <- q == 0 & p > 0; p0 <- p == 0 & q > 0
  out[q0] <- ifelse(z[q0] == 0, 0, stats::pnorm(abs(z[q0]) / p[q0]))
  out[p0] <- ifelse(z[p0] == 0, 0, stats::pnorm(abs(z[p0]) / q[p0]))
  out[p == 0 & q == 0] <- 1
  out
}

# Pr(|OLS| < |IV| and same sign) (vectorized)
.me_Aprob <- function(z, p, q) {
  n <- max(length(z), length(p), length(q))
  z <- rep_len(z, n); p <- rep_len(p, n); q <- rep_len(q, n)
  rho <- p / sqrt(p^2 + q^2)
  a <- -z / p
  out <- rep(NA_real_, n)
  reg <- p > 0 & q > 0
  out[reg] <- 0.5 + .me_bvn0(a[reg], -rho[reg]) - .me_bvn0(a[reg], rho[reg])
  q0 <- q == 0 & p > 0; p0 <- p == 0 & q > 0
  out[q0] <- ifelse(z[q0] == 0, 0, stats::pnorm(abs(z[q0]) / p[q0]) - 0.5)
  out[p0] <- ifelse(z[p0] == 0, 0, 0.5)
  out[p == 0 & q == 0] <- 0
  out
}

# Bivariate standard normal CDF Pr(X <= a, Y <= 0) with correlation r,
# via int_{-inf}^0 phi(y) Phi((a - r y)/sqrt(1 - r^2)) dy (Gauss-Legendre on
# y in (-10, 0)); vectorized over (a, r).
.me_bvn0 <- function(a, r) {
  gl <- .me_gauss_legendre(64)
  y <- -5 * (1 - gl$x)
  wy <- 5 * gl$w * stats::dnorm(y)
  s <- sqrt(1 - r^2)
  arg <- (outer(a, rep(1, length(y))) - outer(r, y)) / s
  drop(stats::pnorm(arg) %*% wy)
}

.me_gauss_legendre <- local({
  cache <- list()
  function(n) {
    key <- as.character(n)
    if (!is.null(cache[[key]])) return(cache[[key]])
    i <- seq_len(n - 1)
    b <- i / sqrt(4 * i^2 - 1)
    J <- matrix(0, n, n)
    J[cbind(i, i + 1)] <- b; J[cbind(i + 1, i)] <- b
    e <- eigen(J, symmetric = TRUE)
    out <- list(x = e$values, w = 2 * e$vectors[1, ]^2)
    cache[[key]] <<- out
    out
  }
})

#' Stated Motivations for Using Instruments
#'
#' Paper-level shares citing omitted variable bias, simultaneity, and
#' measurement error, including all overlaps (the Venn diagram in Figure 2 of
#' Pancost and Schaller, 2026).
#'
#' @param ovb,simultaneity,measurement_error 0/1 vectors, one per paper.
#' @return A data frame of the seven Venn regions plus "none", with counts
#'   and percentages of papers.
#' @family meta_me
#' @export
meta_me_motivation <- function(ovb, simultaneity, measurement_error) {
  o <- ovb %in% 1; s <- simultaneity %in% 1; m <- measurement_error %in% 1
  n <- length(o)
  reg <- list(
    "OVB only" = o & !s & !m, "Simultaneity only" = !o & s & !m,
    "Measurement error only" = !o & !s & m,
    "OVB + simultaneity" = o & s & !m, "Simultaneity + ME" = !o & s & m,
    "OVB + ME" = o & !s & m, "All three" = o & s & m,
    "None stated" = !o & !s & !m)
  data.frame(region = names(reg), n = vapply(reg, sum, 0),
             pct = 100 * vapply(reg, sum, 0) / n, row.names = NULL)
}
