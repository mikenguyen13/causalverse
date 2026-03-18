#' One-Line Tidy Summary for Any Causal Model
#'
#' Extracts a standardised single-row data frame from any causal inference
#' model (lm, glm, fixest, ivreg, etc.). Works via \pkg{broom} if available,
#' with manual fallback extraction for common model classes. A custom S3
#' print method provides a clean, console-friendly summary.
#'
#' @title Unified Causal Model Summary
#'
#' @description Returns a clean one-row data frame with the key causal
#'   estimate, standard error, confidence interval, p-value, number of
#'   observations, and outcome variable name. This is useful for programmatic
#'   comparison of many models.
#'
#' @param model A fitted model object. Supported: `lm`, `glm`, `fixest`,
#'   `ivreg`, and any model class with a `summary()` method that returns a
#'   coefficient table.
#' @param method_name Character or `NULL`. Label for the estimation method.
#'   If `NULL`, the model class is used.
#' @param digits Integer. Number of decimal places for display. Default `3`.
#' @param conf_level Numeric. Confidence level for the interval. Default `0.95`.
#' @param ... Additional arguments passed to `broom::tidy()` if available.
#'
#' @return An object of class `"causal_summary"` inheriting from `"data.frame"`.
#'   Columns: `method`, `term`, `estimate`, `std_error`, `ci_lower`,
#'   `ci_upper`, `p_value`, `n_obs`, `outcome`.
#'
#' @examples
#' set.seed(1)
#' n  <- 300
#' x  <- rnorm(n)
#' y  <- 2 * x + rnorm(n)
#' df <- data.frame(y = y, x = x)
#'
#' m_lm <- lm(y ~ x, data = df)
#' cs   <- causal_summary(m_lm, method_name = "OLS")
#' print(cs)
#'
#' if (requireNamespace("fixest", quietly = TRUE)) {
#'   m_fe <- fixest::feols(y ~ x, data = df)
#'   cs2  <- causal_summary(m_fe, method_name = "OLS (fixest)")
#'   print(cs2)
#' }
#'
#' @importFrom stats coef vcov qt nobs
#' @export
causal_summary <- function(model,
                            method_name = NULL,
                            digits      = 3,
                            conf_level  = 0.95,
                            ...) {

  alpha <- 1 - conf_level
  z_crit <- stats::qnorm(1 - alpha / 2)

  # ---- Determine method label ----------------------------------------------
  cls <- class(model)[1]
  if (is.null(method_name)) method_name <- cls

  # ---- Try broom::tidy() first ----------------------------------------------
  tidy_df <- NULL
  if (requireNamespace("broom", quietly = TRUE)) {
    tidy_df <- tryCatch(
      broom::tidy(model, conf.int = TRUE, conf.level = conf_level, ...),
      error = function(e) NULL
    )
  }

  if (!is.null(tidy_df) && nrow(tidy_df) > 0) {
    # Use broom output - take first non-intercept term if possible
    non_int <- tidy_df[!grepl("^\\(Intercept\\)", tidy_df$term), ]
    row1    <- if (nrow(non_int) > 0) non_int[1, ] else tidy_df[1, ]

    ci_lo <- if ("conf.low"  %in% names(row1)) row1$conf.low  else
               row1$estimate - z_crit * row1$std.error
    ci_hi <- if ("conf.high" %in% names(row1)) row1$conf.high else
               row1$estimate + z_crit * row1$std.error

    res <- data.frame(
      method    = method_name,
      term      = as.character(row1$term),
      estimate  = as.numeric(row1$estimate),
      std_error = as.numeric(if ("std.error" %in% names(row1)) row1$std.error else NA_real_),
      ci_lower  = as.numeric(ci_lo),
      ci_upper  = as.numeric(ci_hi),
      p_value   = as.numeric(if ("p.value" %in% names(row1)) row1$p.value else NA_real_),
      n_obs     = tryCatch(as.integer(stats::nobs(model)), error = function(e) NA_integer_),
      outcome   = .extract_outcome(model),
      stringsAsFactors = FALSE
    )
  } else {
    # ---- Manual fallback ---------------------------------------------------
    cf  <- tryCatch(stats::coef(model), error = function(e) NULL)
    if (is.null(cf) || length(cf) == 0) {
      stop("Cannot extract coefficients from model of class '", cls, "'.")
    }
    # Use first non-intercept or first coefficient
    idx   <- which(!grepl("^\\(Intercept\\)", names(cf)))
    i     <- if (length(idx) > 0) idx[1] else 1
    est   <- unname(cf[i])
    term_nm <- names(cf)[i]

    # Standard error
    se <- tryCatch({
      vc <- stats::vcov(model)
      sqrt(diag(vc)[i])
    }, error = function(e) NA_real_)

    # Degrees of freedom for t-distribution
    df_resid <- tryCatch(model$df.residual, error = function(e) Inf)
    if (is.null(df_resid) || !is.finite(df_resid)) df_resid <- Inf
    t_crit <- stats::qt(1 - alpha / 2, df = df_resid)

    # p-value
    pv <- if (!is.na(se) && se > 0) {
      2 * stats::pt(abs(est / se), df = df_resid, lower.tail = FALSE)
    } else NA_real_

    res <- data.frame(
      method    = method_name,
      term      = term_nm,
      estimate  = est,
      std_error = se,
      ci_lower  = est - t_crit * se,
      ci_upper  = est + t_crit * se,
      p_value   = pv,
      n_obs     = tryCatch(as.integer(stats::nobs(model)), error = function(e) NA_integer_),
      outcome   = .extract_outcome(model),
      stringsAsFactors = FALSE
    )
  }

  attr(res, "digits") <- digits
  class(res) <- c("causal_summary", "data.frame")
  res
}

# ---- Internal helper: extract outcome name ----------------------------------
.extract_outcome <- function(model) {
  fml <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (!is.null(fml)) {
    lhs <- deparse(fml[[2]])
    return(lhs)
  }
  NA_character_
}

#' Print Method for causal_summary Objects
#'
#' @param x A `causal_summary` object.
#' @param ... Currently ignored.
#' @export
#' @noRd
print.causal_summary <- function(x, ...) {
  dg <- attr(x, "digits")
  if (is.null(dg)) dg <- 3

  cat("\n=== Causal Summary ===\n")
  cat(sprintf("  Method  : %s\n", x$method))
  cat(sprintf("  Outcome : %s\n", x$outcome))
  cat(sprintf("  Term    : %s\n", x$term))
  cat(sprintf("  Estimate: %s  (SE: %s)\n",
              format(round(x$estimate, dg), nsmall = dg),
              format(round(x$std_error, dg), nsmall = dg)))
  cat(sprintf("  95%% CI  : [%s, %s]\n",
              format(round(x$ci_lower, dg), nsmall = dg),
              format(round(x$ci_upper, dg), nsmall = dg)))
  cat(sprintf("  p-value : %s\n",
              format(round(x$p_value, max(dg, 4)), nsmall = max(dg, 4))))
  cat(sprintf("  N obs   : %s\n", x$n_obs))
  cat("======================\n\n")
  invisible(x)
}
