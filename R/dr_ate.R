#' Doubly-Robust Augmented IPW Estimator for the ATE and ATT
#'
#' Estimates the Average Treatment Effect (ATE) or Average Treatment effect
#' on the Treated (ATT) using the Augmented Inverse Probability Weighting
#' (AIPW) estimator, which is doubly robust: consistent if either the
#' propensity score model or the outcome model is correctly specified.
#'
#' @param data A data frame.
#' @param outcome Character. Name of the outcome variable.
#' @param treatment Character. Name of the binary treatment variable (0/1).
#' @param covariates Character vector. Names of covariates to use in both the
#'   propensity score and outcome models.
#' @param estimand Character. Either \code{"ATE"} (default) or \code{"ATT"}.
#' @param ps_formula Formula or \code{NULL}. Custom formula for the propensity
#'   score model (logistic regression). If \code{NULL}, a main-effects logistic
#'   regression on \code{covariates} is used.
#' @param out_formula Formula or \code{NULL}. Custom formula for the outcome
#'   model (linear regression). If \code{NULL}, a main-effects linear
#'   regression on \code{treatment} and \code{covariates} is used.
#' @param ps_trim Numeric vector of length 2. Propensity scores outside this
#'   range are trimmed. Default \code{c(0.01, 0.99)}.
#' @param boot_se Logical. If \code{TRUE}, compute bootstrap standard errors.
#'   Default \code{FALSE}.
#' @param n_boot Integer. Number of bootstrap replications. Default \code{500}.
#' @param seed Integer. Random seed for reproducibility. Default \code{42}.
#' @param conf_level Numeric. Confidence level. Default \code{0.95}.
#'
#' @return A list with:
#'   \describe{
#'     \item{estimate}{Numeric. AIPW point estimate.}
#'     \item{se}{Numeric. Influence-function standard error (or bootstrap SE).}
#'     \item{ci_lower}{Numeric. Lower confidence bound.}
#'     \item{ci_upper}{Numeric. Upper confidence bound.}
#'     \item{t_stat}{Numeric. t-statistic.}
#'     \item{p_value}{Numeric. Two-sided p-value.}
#'     \item{estimand}{Character. \code{"ATE"} or \code{"ATT"}.}
#'     \item{n_trimmed}{Integer. Number of observations trimmed due to extreme PS.}
#'     \item{ps_summary}{Named vector. Summary statistics of propensity scores.}
#'   }
#'
#' @details
#' The AIPW estimator for the ATE is:
#' \deqn{\hat{\tau}_{AIPW} = \frac{1}{n}\sum_{i=1}^{n}\left[
#'   \mu_1(X_i) - \mu_0(X_i) +
#'   \frac{D_i(Y_i - \mu_1(X_i))}{e(X_i)} -
#'   \frac{(1-D_i)(Y_i - \mu_0(X_i))}{1-e(X_i)}
#' \right]}
#' where \eqn{e(X) = P(D=1|X)} is the propensity score, and
#' \eqn{\mu_d(X) = E[Y|D=d,X]} is the conditional outcome mean.
#'
#' @references
#' Robins, J. M., Rotnitzky, A., & Zhao, L. P. (1994). Estimation of
#' regression coefficients when some regressors are not always observed.
#' \emph{Journal of the American Statistical Association}, 89(427), 846–866.
#'
#' @examples
#' data(lalonde, package = "MatchIt")
#' result <- dr_ate(
#'   data       = lalonde,
#'   outcome    = "re78",
#'   treatment  = "treat",
#'   covariates = c("age", "educ", "race", "married", "nodegree", "re74", "re75")
#' )
#' result
#'
#' @importFrom stats glm binomial lm predict formula as.formula qnorm pnorm
#' @export
dr_ate <- function(data,
                   outcome,
                   treatment,
                   covariates,
                   estimand   = c("ATE", "ATT"),
                   ps_formula  = NULL,
                   out_formula = NULL,
                   ps_trim     = c(0.01, 0.99),
                   boot_se     = FALSE,
                   n_boot      = 500,
                   seed        = 42,
                   conf_level  = 0.95) {

  estimand <- match.arg(estimand)

  # --- Validate inputs ---
  required <- c(outcome, treatment, covariates)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Columns not found in data: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  data[[treatment]] <- as.integer(as.logical(data[[treatment]]))
  n_total <- nrow(data)

  # --- Propensity score model ---
  if (is.null(ps_formula)) {
    ps_formula <- stats::as.formula(
      paste(treatment, "~", paste(covariates, collapse = " + "))
    )
  }
  ps_model <- stats::glm(ps_formula, data = data, family = stats::binomial())
  ps <- stats::predict(ps_model, type = "response")

  # Trim propensity scores
  ps_raw <- ps
  ps <- pmax(pmin(ps, ps_trim[2]), ps_trim[1])
  n_trimmed <- sum(ps_raw < ps_trim[1] | ps_raw > ps_trim[2])

  # --- Outcome models (separate for each arm) ---
  if (is.null(out_formula)) {
    out_rhs <- paste(c(treatment, covariates), collapse = " + ")
    out_formula <- stats::as.formula(paste(outcome, "~", out_rhs))
  }

  # Fit outcome model on full data
  out_model <- stats::lm(out_formula, data = data)

  # Predict potential outcomes
  d1 <- d0 <- data
  d1[[treatment]] <- 1L
  d0[[treatment]] <- 0L
  mu1 <- stats::predict(out_model, newdata = d1)
  mu0 <- stats::predict(out_model, newdata = d0)

  Y <- data[[outcome]]
  D <- data[[treatment]]

  # --- AIPW influence function ---
  compute_aipw <- function(Y, D, mu1, mu0, ps, estimand) {
    if (estimand == "ATE") {
      psi <- (mu1 - mu0) +
        D * (Y - mu1) / ps -
        (1 - D) * (Y - mu0) / (1 - ps)
    } else {
      # ATT
      p_treat <- mean(D)
      psi <- (D * (Y - mu0) / ps - (1 - D) * (Y - mu0) * ps / ((1 - ps) * p_treat)) +
        (mu1 - mu0) * D / p_treat - mean((mu1 - mu0) * D) / p_treat
    }
    list(
      estimate = mean(psi),
      se       = stats::sd(psi) / sqrt(length(psi))
    )
  }

  res <- compute_aipw(Y, D, mu1, mu0, ps, estimand)
  est <- res$estimate
  se  <- res$se

  # --- Optional bootstrap SE ---
  if (boot_se) {
    set.seed(seed)
    boot_ests <- numeric(n_boot)
    for (b in seq_len(n_boot)) {
      idx <- sample(nrow(data), replace = TRUE)
      db  <- data[idx, , drop = FALSE]

      ps_b  <- tryCatch({
        m  <- stats::glm(ps_formula, data = db, family = stats::binomial())
        p  <- stats::predict(m, newdata = data, type = "response")
        pmax(pmin(p, ps_trim[2]), ps_trim[1])
      }, error = function(e) ps)

      out_b <- tryCatch({
        stats::lm(out_formula, data = db)
      }, error = function(e) out_model)

      mu1_b <- stats::predict(out_b, newdata = d1)
      mu0_b <- stats::predict(out_b, newdata = d0)

      boot_ests[b] <- compute_aipw(Y, D, mu1_b, mu0_b, ps_b, estimand)$estimate
    }
    se <- stats::sd(boot_ests)
  }

  # --- Inference ---
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  t_stat  <- est / se
  p_value <- 2 * stats::pnorm(-abs(t_stat))

  list(
    estimate   = est,
    se         = se,
    ci_lower   = est - z * se,
    ci_upper   = est + z * se,
    t_stat     = t_stat,
    p_value    = p_value,
    estimand   = estimand,
    n_trimmed  = n_trimmed,
    n_total    = n_total,
    ps_summary = summary(ps_raw)
  )
}
