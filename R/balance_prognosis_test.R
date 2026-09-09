#' Prognosis-Weighted Covariate Balance Test
#'
#' Omnibus test of as-if random assignment that weights each covariate by how
#' strongly it predicts the outcome, following Bicalho, Bouyamourn, and Dunning
#' (2026). Standard balance tests treat every measured covariate as equally
#' informative. When the covariates in the table are unrelated to potential
#' outcomes, balance on them says nothing about whether assignment was
#' independent of potential outcomes, so the test is prone to both false
#' negatives (failing to reject a design that is in fact broken) and false
#' positives (rejecting because irrelevant noise covariates happen to be
#' imbalanced). This function downweights uninformative covariates and
#' prioritises the ones that carry outcome information, returning a single
#' statistic with a single p-value.
#'
#' @section Setup and notation:
#' Let \eqn{D_i \in \{0, 1\}} be the treatment indicator for unit
#' \eqn{i = 1, \dots, n}, with \eqn{n_1} treated and \eqn{n_0} control units.
#' Let \eqn{X_i \in \mathbb{R}^p} be the measured pre-treatment covariates and
#' \eqn{Y_i} the observed outcome, with potential outcomes \eqn{Y_i(0)} and
#' \eqn{Y_i(1)}. The identification condition under test is as-if random
#' assignment,
#' \deqn{D \perp\!\!\!\perp (Y(0), Y(1)),}
#' which cannot be checked directly because only one potential outcome is
#' observed per unit.
#'
#' @section The prognostic score:
#' Fit the outcome model on the control units only, where the observed outcome
#' equals \eqn{Y_i(0)}:
#' \deqn{\hat{m}(x) = \hat{\alpha} + \hat{\beta}^\top x, \qquad
#'   (\hat{\alpha}, \hat{\beta}) = \arg\min_{a, b}
#'   \sum_{i : D_i = 0} (Y_i - a - b^\top X_i)^2 .}
#' \eqn{\hat{m}(X_i)} is Hansen's (2008) prognostic score: the projection of the
#' control potential outcome onto the covariates. It is defined for treated and
#' control units alike, because it is a function of \eqn{X_i} only.
#'
#' @section The test statistic:
#' The prognosis-weighted statistic is the difference in the average fitted
#' control potential outcome across arms, scaled by the control-group outcome
#' standard deviation \eqn{s_{Y(0)}}:
#' \deqn{\delta_{PW} = \frac{1}{s_{Y(0)}} \left(
#'   \frac{1}{n_1} \sum_{i : D_i = 1} \hat{m}(X_i)
#'   - \frac{1}{n_0} \sum_{i : D_i = 0} \hat{m}(X_i) \right).}
#' Because \eqn{\hat{m}} is linear, the intercept cancels and this is
#' algebraically identical to a prognosis-weighted sum of covariate mean
#' differences,
#' \deqn{\delta_{PW} = \sum_{k=1}^{p} b_k d_k, \qquad
#'   b_k = \frac{\hat{\beta}_k s_k}{s_{Y(0)}}, \qquad
#'   d_k = \frac{\bar{X}_{1k} - \bar{X}_{0k}}{s_k},}
#' where \eqn{s_k = \sqrt{(s_{1k}^2 + s_{0k}^2)/2}} is the pooled standard
#' deviation used as the denominator of the standardised mean difference
#' elsewhere in this package (see \code{\link{balance_table}}). The term
#' \eqn{b_k} is the standardised prognosis coefficient — how much of an outcome
#' standard deviation a one-standard-deviation move in \eqn{X_k} predicts — and
#' \eqn{d_k} is the usual standardised mean difference. So the statistic takes
#' the ordinary balance table as input and collapses it into one number by
#' weighting each row by that covariate's prognostic strength. The scaling
#' \eqn{s_k} cancels in the product, so \eqn{\delta_{PW}} itself does not depend
#' on that convention; only the reported decomposition does.
#'
#' The unweighted comparator reported alongside it is the sum of standardised
#' mean differences, \eqn{\delta_{UW} = \sum_k d_k}, which is what a balance
#' test that treats all covariates equally amounts to.
#'
#' @section Measuring prognosis:
#' The \eqn{R^2} of the control-group outcome regression is the joint prognosis
#' of the covariate set. Bicalho et al. find that in published natural
#' experiments this quantity is rarely reported and is frequently near zero, in
#' which case the balance test — prognosis-weighted or not — carries almost no
#' information about the identification condition. Their simulations suggest
#' reasonable performance from a prognosis \eqn{R^2} of roughly 0.1 to 0.2
#' upward; this function warns below \code{r2_warn}. The per-covariate
#' \eqn{(b_k, d_k)} pairs in \code{$coefficients} are the ingredients of their
#' Figure 2 diagnostic: prognosis on one axis, imbalance on the other.
#'
#' @section Inference:
#' The prognosis weights \eqn{\hat{\beta}} and the covariate mean differences
#' \eqn{\bar{X}_1 - \bar{X}_0} are estimated from the same data and are
#' statistically dependent, so the reference distribution is resampling-based
#' and recomputes both inside every replicate.
#'
#' \code{method = "bootstrap"} (the default) resamples units with replacement
#' within each treatment arm, so the arm sizes are held fixed, and recomputes
#' \eqn{\delta_{PW}^*}. The null distribution is obtained by recentring,
#' \eqn{\delta_{PW}^* - \hat{\delta}_{PW}}, and the two-sided p-value is
#' \eqn{B^{-1} \sum_b \mathbb{1}\{|\delta^*_{PW,b} - \hat{\delta}_{PW}| \ge
#' |\hat{\delta}_{PW}|\}}.
#'
#' \code{method = "permutation"} instead permutes the treatment vector holding
#' \eqn{n_1} fixed, refits the prognosis regression on the permuted control
#' group, and compares \eqn{|\hat{\delta}_{PW}|} to the permutation
#' distribution. This is randomisation inference directly against the sharp null
#' of as-if random assignment and does not rely on an asymptotic argument; it is
#' the more conservative choice in small samples.
#'
#' With \code{cluster} supplied, whole clusters are resampled (bootstrap) or
#' whole clusters have their treatment status permuted (permutation), which is
#' the adaptation Bicalho et al. describe for clustered and blocked designs.
#'
#' @section Assumptions:
#' \enumerate{
#'   \item \strong{Pre-treatment covariates.} \code{covariates} must be fixed
#'     before assignment. Post-treatment covariates are affected by treatment,
#'     and conditioning on them makes the test meaningless rather than merely
#'     weak.
#'   \item \strong{Outcome observed under control.} The prognosis regression is
#'     fit on control units, where \eqn{Y_i = Y_i(0)}. If the control arm is
#'     itself contaminated by treatment, the prognostic score is not a model of
#'     \eqn{Y(0)}.
#'   \item \strong{Prognosis is estimable.} \eqn{n_0} must exceed the number of
#'     terms in the design matrix, and the covariates must have some predictive
#'     content. A near-zero prognosis \eqn{R^2} does not invalidate the p-value
#'     but does make it uninformative about the identification condition, which
#'     is the paper's central warning.
#' }
#' Note what the test does \emph{not} assume: it does not require the outcome
#' model to be correctly specified. Misspecification costs power, because the
#' weights are then a poorer summary of prognosis, but under as-if random
#' assignment the resampling reference distribution is still valid, since
#' \eqn{\hat{\beta}} is a function of the data that is recomputed identically in
#' every replicate.
#'
#' @param data A data frame containing the treatment indicator, covariates, and
#'   outcome.
#' @param treatment Character. Name of the binary treatment indicator column
#'   (0/1, logical, or a two-level factor).
#' @param outcome Character. Name of the outcome column. Used only to estimate
#'   prognosis, on control units.
#' @param covariates Character vector. Names of the pre-treatment covariate
#'   columns. If \code{NULL} (default), all numeric and logical columns other
#'   than \code{treatment} and \code{outcome} are used.
#' @param basis Character. Design matrix used in the prognosis regression:
#'   \code{"linear"} (default), \code{"poly"} (adds squared terms for
#'   covariates with more than two distinct values), \code{"interaction"}
#'   (adds all pairwise products), or \code{"full"} (both). Bicalho et al.
#'   find expanded linear bases to be the best-performing option overall,
#'   beating machine-learning fits in their simulations, but they cost degrees
#'   of freedom and need a larger control group.
#' @param method Character. Reference distribution: \code{"bootstrap"}
#'   (default) or \code{"permutation"}. See Inference.
#' @param n_boot Integer. Number of resampling draws. Default \code{1000}.
#' @param cluster Optional vector of length \code{nrow(data)}, or the name of a
#'   column in \code{data}, giving cluster membership. Resampling is then done
#'   over whole clusters.
#' @param alpha Numeric. Significance level for the bootstrap confidence
#'   interval. Default \code{0.05}. Ignored when
#'   \code{method = "permutation"}.
#' @param r2_warn Numeric. Warn when the prognosis \eqn{R^2} falls below this
#'   value, indicating that the covariates carry little outcome information and
#'   the balance test is correspondingly uninformative. Default \code{0.1}. Set
#'   to \code{0} to disable.
#' @param seed Optional integer. Seed for the resampling draws, for
#'   reproducibility. If \code{NULL} (default) the ambient RNG state is used.
#'
#' @return A list with components:
#'   \describe{
#'     \item{\code{delta_pw}}{The prognosis-weighted statistic
#'       \eqn{\hat{\delta}_{PW}}, in control-outcome standard deviations.}
#'     \item{\code{p_value}}{Two-sided p-value for \eqn{\hat{\delta}_{PW}}.}
#'     \item{\code{delta_uw}}{The unweighted comparator \eqn{\delta_{UW}}.}
#'     \item{\code{p_value_uw}}{Two-sided p-value for \eqn{\delta_{UW}} from
#'       the same resampling draws.}
#'     \item{\code{prognosis_r2}}{\eqn{R^2} of the control-group outcome
#'       regression: the joint prognosis of the covariate set.}
#'     \item{\code{prognosis_rmse}}{Residual standard error of that regression,
#'       the related fit measure Bicalho et al. also recommend reporting.}
#'     \item{\code{coefficients}}{Data frame with one row per term in the
#'       design matrix: \code{term}, standardised prognosis coefficient
#'       \code{prognosis} (\eqn{b_k}), standardised mean difference \code{smd}
#'       (\eqn{d_k}), and \code{contribution} (\eqn{b_k d_k}, which sums to
#'       \code{delta_pw}). Sorted by \code{abs(contribution)}, descending.}
#'     \item{\code{ci}}{Named numeric vector with the bootstrap percentile
#'       confidence bounds for \eqn{\delta_{PW}}. \code{NULL} when
#'       \code{method = "permutation"}.}
#'     \item{\code{se}}{Resampling standard deviation of
#'       \eqn{\delta_{PW}^*}.}
#'     \item{\code{draws}}{Numeric vector of the \code{n_boot} realised values
#'       of \eqn{\delta_{PW}^*}.}
#'     \item{\code{n_treated}, \code{n_control}}{Arm sizes after any listwise
#'       deletion.}
#'     \item{\code{n_dropped}}{Number of rows dropped for missingness.}
#'     \item{\code{n_failed}}{Number of resampling draws that could not be
#'       evaluated (for example a degenerate resample) and were excluded from
#'       the p-value.}
#'     \item{\code{method}, \code{basis}, \code{n_boot}}{The settings used.}
#'   }
#'
#' @references
#' Bicalho, C., Bouyamourn, A., & Dunning, T. (2026). The Power of Prognosis:
#' Improving Covariate Balance Tests with Outcome Information.
#' \emph{Political Analysis}, 1--26. \doi{10.1017/pan.2026.10050}
#'
#' Hansen, B. B. (2008). The prognostic analogue of the propensity score.
#' \emph{Biometrika}, 95(2), 481--488. \doi{10.1093/biomet/asn004}
#'
#' Stuart, E. A., Lee, B. K., & Leacy, F. P. (2013). Prognostic score-based
#' balance measures can be a useful diagnostic for propensity score methods in
#' comparative effectiveness research. \emph{Journal of Clinical Epidemiology},
#' 66(8), S84--S90. \doi{10.1016/j.jclinepi.2013.01.013}
#'
#' @seealso \code{\link{balance_assessment}} for the unweighted SUR and
#'   Hotelling's \eqn{T^2} joint tests, \code{\link{balance_table}} for the
#'   covariate-by-covariate table whose rows this statistic weights, and
#'   \code{\link{love_plot}} for the standard graphical summary.
#'
#' @examples
#' set.seed(1)
#' n <- 300
#' # Two prognostic covariates and three pure noise covariates.
#' d <- data.frame(
#'   x1 = rnorm(n), x2 = rnorm(n),
#'   z1 = rnorm(n), z2 = rnorm(n), z3 = rnorm(n)
#' )
#' # As-if random fails: assignment depends on the prognostic x1.
#' d$treat <- rbinom(n, 1, plogis(0.9 * d$x1))
#' d$y <- 1.5 * d$x1 - 1.0 * d$x2 + rnorm(n)
#'
#' res <- balance_prognosis_test(
#'   data       = d,
#'   treatment  = "treat",
#'   outcome    = "y",
#'   covariates = c("x1", "x2", "z1", "z2", "z3"),
#'   n_boot     = 200,
#'   seed       = 42
#' )
#' res$prognosis_r2
#' res$delta_pw
#' res$p_value
#' # The noise covariates carry almost no weight:
#' res$coefficients
#'
#' @importFrom stats sd var quantile complete.cases rbinom
#'   rnorm plogis
#' @export
balance_prognosis_test <- function(data,
                                   treatment,
                                   outcome,
                                   covariates = NULL,
                                   basis      = c("linear", "poly",
                                                  "interaction", "full"),
                                   method     = c("bootstrap", "permutation"),
                                   n_boot     = 1000,
                                   cluster    = NULL,
                                   alpha      = 0.05,
                                   r2_warn    = 0.1,
                                   seed       = NULL) {

  basis  <- match.arg(basis)
  method <- match.arg(method)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(treatment) || length(treatment) != 1L) {
    stop("`treatment` must be a single column name.", call. = FALSE)
  }
  if (!is.character(outcome) || length(outcome) != 1L) {
    stop("`outcome` must be a single column name.", call. = FALSE)
  }
  if (!treatment %in% names(data)) {
    stop("`treatment` column '", treatment, "' not found in data.",
         call. = FALSE)
  }
  if (!outcome %in% names(data)) {
    stop("`outcome` column '", outcome, "' not found in data.", call. = FALSE)
  }
  if (!is.numeric(n_boot) || length(n_boot) != 1L || n_boot < 1) {
    stop("`n_boot` must be a positive integer.", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)

  # ---- cluster argument: column name or vector ------------------------------
  if (!is.null(cluster)) {
    if (is.character(cluster) && length(cluster) == 1L &&
        cluster %in% names(data)) {
      cluster <- data[[cluster]]
    }
    if (length(cluster) != nrow(data)) {
      stop("`cluster` must be a column name in `data` or a vector of length ",
           "nrow(data).", call. = FALSE)
    }
  }

  # ---- treatment indicator --------------------------------------------------
  treat_raw <- data[[treatment]]
  if (is.factor(treat_raw)) {
    if (nlevels(treat_raw) != 2L) {
      stop("`treatment` must be binary; found ", nlevels(treat_raw),
           " levels.", call. = FALSE)
    }
    treat_vec <- as.integer(treat_raw) - 1L
  } else {
    treat_vec <- as.integer(as.logical(treat_raw))
  }
  if (anyNA(treat_vec)) {
    stop("`treatment` contains values that are neither 0/1, logical, nor a ",
         "two-level factor.", call. = FALSE)
  }
  if (length(unique(treat_vec)) != 2L) {
    stop("`treatment` must take both values 0 and 1.", call. = FALSE)
  }

  # ---- covariates -----------------------------------------------------------
  if (is.null(covariates)) {
    covariates <- setdiff(
      names(data)[vapply(data, function(x) is.numeric(x) || is.logical(x),
                         logical(1))],
      c(treatment, outcome)
    )
  }
  missing_vars <- setdiff(covariates, names(data))
  if (length(missing_vars) > 0) {
    stop("Covariates not found: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  }
  if (length(covariates) == 0L) {
    stop("No covariates to test.", call. = FALSE)
  }

  y_vec <- data[[outcome]]
  if (!is.numeric(y_vec)) {
    stop("`outcome` column '", outcome, "' must be numeric.", call. = FALSE)
  }

  X_raw <- data[, covariates, drop = FALSE]
  not_numeric <- names(X_raw)[!vapply(X_raw, function(x) {
    is.numeric(x) || is.logical(x)
  }, logical(1))]
  if (length(not_numeric) > 0) {
    stop("Covariates must be numeric or logical. Non-numeric: ",
         paste(not_numeric, collapse = ", "),
         ". Convert factors to dummies first (see fastDummies::dummy_cols).",
         call. = FALSE)
  }
  X_raw <- as.data.frame(lapply(X_raw, as.numeric), stringsAsFactors = FALSE)
  names(X_raw) <- covariates

  # ---- listwise deletion ----------------------------------------------------
  keep <- stats::complete.cases(X_raw, y_vec, treat_vec)
  n_dropped <- sum(!keep)
  if (n_dropped > 0) {
    warning(n_dropped, " row(s) dropped for missing values in the treatment, ",
            "outcome, or covariates. Bicalho et al. (2026, sec. 8.2) note ",
            "that listwise deletion can change balance-test conclusions ",
            "materially; consider imputing or restricting the covariate set.",
            call. = FALSE)
  }
  X_raw     <- X_raw[keep, , drop = FALSE]
  y_vec     <- y_vec[keep]
  treat_vec <- treat_vec[keep]
  if (!is.null(cluster)) cluster <- cluster[keep]

  if (length(unique(treat_vec)) != 2L) {
    stop("After dropping incomplete rows, `treatment` no longer takes both ",
         "values.", call. = FALSE)
  }

  # ---- basis expansion ------------------------------------------------------
  X <- .bpt_expand_basis(X_raw, basis)

  # Drop constant columns: they carry neither prognosis nor imbalance.
  const <- vapply(as.data.frame(X), function(x) {
    stats::var(x) < .Machine$double.eps
  }, logical(1))
  if (any(const)) {
    warning("Dropped ", sum(const), " constant term(s): ",
            paste(colnames(X)[const], collapse = ", "), ".", call. = FALSE)
    X <- X[, !const, drop = FALSE]
  }
  if (ncol(X) == 0L) {
    stop("No non-constant covariate terms remain.", call. = FALSE)
  }

  n0 <- sum(treat_vec == 0)
  n1 <- sum(treat_vec == 1)
  if (n0 <= ncol(X) + 1L) {
    stop("The prognosis regression needs more control units than terms: ",
         n0, " control unit(s) for ", ncol(X), " term(s). Use ",
         "basis = \"linear\" or a smaller covariate set.", call. = FALSE)
  }

  # ---- point estimate -------------------------------------------------------
  fit <- .bpt_statistic(X, y_vec, treat_vec, want_fit = TRUE)
  if (is.null(fit)) {
    stop("The prognosis regression could not be estimated on the observed ",
         "data.", call. = FALSE)
  }

  if (length(fit$aliased) > 0) {
    warning("Dropped ", length(fit$aliased), " collinear term(s) from the ",
            "prognosis regression: ", paste(fit$aliased, collapse = ", "),
            ".", call. = FALSE)
  }
  if (r2_warn > 0 && fit$r2 < r2_warn) {
    warning("Prognosis R-squared is ", format(round(fit$r2, 4)),
            ", below r2_warn = ", r2_warn, ". The covariates carry little ",
            "information about the control potential outcome, so this ",
            "balance test -- weighted or unweighted -- says little about ",
            "as-if random assignment (Bicalho et al. 2026). Collect more ",
            "prognostic covariates, such as a lagged outcome, before reading ",
            "much into the p-value.", call. = FALSE)
  }

  # ---- reference distribution ----------------------------------------------
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L) {
      stop("`seed` must be a single number or NULL.", call. = FALSE)
    }
    set.seed(as.integer(seed))
  }

  cl_id  <- if (is.null(cluster)) NULL else as.character(cluster)
  draws  <- rep(NA_real_, n_boot)
  draws_uw <- rep(NA_real_, n_boot)

  for (b in seq_len(n_boot)) {
    idx <- if (method == "bootstrap") {
      .bpt_resample(treat_vec, cl_id)
    } else {
      NULL
    }

    if (method == "bootstrap") {
      if (is.null(idx)) next
      rep_stat <- .bpt_statistic(X[idx, , drop = FALSE], y_vec[idx],
                                 treat_vec[idx])
    } else {
      d_star   <- .bpt_permute(treat_vec, cl_id)
      rep_stat <- if (is.null(d_star)) NULL else {
        .bpt_statistic(X, y_vec, d_star)
      }
    }

    if (!is.null(rep_stat)) {
      draws[b]    <- rep_stat$delta_pw
      draws_uw[b] <- rep_stat$delta_uw
    }
  }

  ok       <- !is.na(draws)
  n_failed <- sum(!ok)
  if (sum(ok) < 2L) {
    stop("Fewer than two resampling draws could be evaluated; the ",
         "reference distribution is undefined. Check for near-collinear ",
         "covariates or very small arms.", call. = FALSE)
  }
  if (n_failed > 0) {
    warning(n_failed, " of ", n_boot, " resampling draw(s) could not be ",
            "evaluated and were excluded from the p-value.", call. = FALSE)
  }

  draws    <- draws[ok]
  draws_uw <- draws_uw[ok]

  if (method == "bootstrap") {
    # Recentre the bootstrap distribution to impose the null.
    null_pw <- draws - fit$delta_pw
    null_uw <- draws_uw - fit$delta_uw
    ci <- stats::quantile(draws, probs = c(alpha / 2, 1 - alpha / 2),
                          names = FALSE, na.rm = TRUE)
    ci <- c(lower = ci[1], upper = ci[2])
  } else {
    # Under permutation the draws are already null draws.
    null_pw <- draws
    null_uw <- draws_uw
    ci <- NULL
  }

  p_pw <- mean(abs(null_pw) >= abs(fit$delta_pw))
  p_uw <- mean(abs(null_uw) >= abs(fit$delta_uw))

  coefs <- data.frame(
    term         = fit$terms,
    prognosis    = fit$b,
    smd          = fit$d,
    contribution = fit$b * fit$d,
    stringsAsFactors = FALSE
  )
  coefs <- coefs[order(abs(coefs$contribution), decreasing = TRUE), ,
                 drop = FALSE]
  rownames(coefs) <- NULL

  list(
    delta_pw        = fit$delta_pw,
    p_value         = p_pw,
    delta_uw        = fit$delta_uw,
    p_value_uw      = p_uw,
    prognosis_r2    = fit$r2,
    prognosis_rmse  = fit$rmse,
    coefficients    = coefs,
    ci              = ci,
    se              = stats::sd(draws),
    draws           = draws,
    n_treated       = n1,
    n_control       = n0,
    n_dropped       = n_dropped,
    n_failed        = n_failed,
    method          = method,
    basis           = basis,
    n_boot          = n_boot
  )
}


#' Expand a covariate matrix into a polynomial and/or interaction basis
#'
#' Internal helper for \code{\link{balance_prognosis_test}}. Squared terms are
#' added only for covariates with more than two distinct values, since the
#' square of a binary covariate is the covariate itself.
#'
#' @param X_raw Data frame of numeric covariates.
#' @param basis One of \code{"linear"}, \code{"poly"}, \code{"interaction"},
#'   \code{"full"}.
#'
#' @return A numeric matrix with named columns.
#' @keywords internal
#' @noRd
.bpt_expand_basis <- function(X_raw, basis) {

  X <- as.matrix(X_raw)
  if (basis == "linear") return(X)

  nm    <- colnames(X)
  extra <- list()

  if (basis %in% c("poly", "full")) {
    for (k in seq_along(nm)) {
      if (length(unique(X[, k])) > 2L) {
        extra[[paste0("I(", nm[k], "^2)")]] <- X[, k]^2
      }
    }
  }

  if (basis %in% c("interaction", "full") && length(nm) >= 2L) {
    for (k in seq_len(length(nm) - 1L)) {
      for (j in seq(k + 1L, length(nm))) {
        extra[[paste0(nm[k], ":", nm[j])]] <- X[, k] * X[, j]
      }
    }
  }

  if (length(extra) == 0L) return(X)
  cbind(X, do.call(cbind, extra))
}


#' Compute the prognosis-weighted and unweighted balance statistics
#'
#' Internal workhorse for \code{\link{balance_prognosis_test}}. Fits the
#' control-group outcome regression, forms the standardised prognosis
#' coefficients and standardised mean differences, and returns their inner
#' product. Returns \code{NULL} rather than erroring when the input is
#' degenerate, so that a bad resampling draw can simply be dropped.
#'
#' @param X Numeric design matrix.
#' @param y Numeric outcome vector.
#' @param d Integer 0/1 treatment vector.
#' @param want_fit Logical. If \code{TRUE}, also return the fit diagnostics and
#'   per-term decomposition.
#'
#' @return A list, or \code{NULL} if the statistic is not computable.
#' @keywords internal
#' @noRd
.bpt_statistic <- function(X, y, d, want_fit = FALSE) {

  i0 <- which(d == 0)
  i1 <- which(d == 1)
  if (length(i0) < 2L || length(i1) < 2L) return(NULL)

  X0 <- X[i0, , drop = FALSE]
  X1 <- X[i1, , drop = FALSE]
  y0 <- y[i0]

  s_y0 <- stats::sd(y0)
  if (!is.finite(s_y0) || s_y0 < .Machine$double.eps) return(NULL)

  # Pooled standardised mean difference denominator, matching balance_table().
  s1  <- .bpt_colsd(X1)
  s0  <- .bpt_colsd(X0)
  s_k <- sqrt((s1^2 + s0^2) / 2)
  if (any(!is.finite(s_k)) || any(s_k < .Machine$double.eps)) return(NULL)

  d_k <- (colMeans(X1) - colMeans(X0)) / s_k

  # Prognosis regression on control units only, where Y = Y(0). Solved through
  # the QR factorisation rather than lm(), both for speed inside the resampling
  # loop and to avoid the formula interface mangling expanded basis names such
  # as "I(x1^2)" and "x1:x2".
  Xd <- cbind(1, X0)
  colnames(Xd) <- c("(Intercept)", colnames(X0))

  qrx <- try(qr(Xd), silent = TRUE)
  if (inherits(qrx, "try-error") || qrx$rank < 2L) return(NULL)

  aliased <- character(0)
  if (qrx$rank < ncol(Xd)) {
    aliased <- colnames(Xd)[qrx$pivot[seq.int(qrx$rank + 1L, ncol(Xd))]]
  }

  cf <- try(qr.coef(qrx, y0), silent = TRUE)
  if (inherits(cf, "try-error")) return(NULL)

  beta <- cf[-1L]
  beta[is.na(beta)] <- 0
  if (length(beta) != ncol(X)) return(NULL)

  b_k <- as.numeric(beta) * s_k / s_y0

  delta_pw <- sum(b_k * d_k)
  delta_uw <- sum(d_k)
  if (!is.finite(delta_pw) || !is.finite(delta_uw)) return(NULL)

  out <- list(delta_pw = delta_pw, delta_uw = delta_uw)

  if (want_fit) {
    res <- qr.resid(qrx, y0)
    rss <- sum(res^2)
    tss <- sum((y0 - mean(y0))^2)
    dfr <- length(y0) - qrx$rank

    out$r2      <- if (tss > 0) 1 - rss / tss else NA_real_
    out$rmse    <- if (dfr > 0) sqrt(rss / dfr) else NA_real_
    out$b       <- b_k
    out$d       <- as.numeric(d_k)
    out$terms   <- colnames(X)
    out$aliased <- aliased
  }

  out
}


#' Column standard deviations of a numeric matrix
#'
#' Internal helper for \code{\link{balance_prognosis_test}}: a vectorised
#' replacement for \code{apply(X, 2, sd)}, which is called once per resampling
#' draw.
#'
#' @param M A numeric matrix with at least two rows.
#'
#' @return A numeric vector of column standard deviations.
#' @keywords internal
#' @noRd
.bpt_colsd <- function(M) {
  n  <- nrow(M)
  mu <- colMeans(M)
  sqrt(colSums((M - rep(mu, each = n))^2) / (n - 1L))
}


#' Draw a stratified or clustered bootstrap resample
#'
#' Internal helper for \code{\link{balance_prognosis_test}}. Resamples with
#' replacement within treatment arm, so arm sizes are preserved; with clusters,
#' resamples whole clusters within arm.
#'
#' @param d Integer 0/1 treatment vector.
#' @param cl Character vector of cluster ids, or \code{NULL}.
#'
#' @return An integer vector of row indices, or \code{NULL} if degenerate.
#' @keywords internal
#' @noRd
.bpt_resample <- function(d, cl) {

  if (is.null(cl)) {
    i0 <- which(d == 0)
    i1 <- which(d == 1)
    return(c(sample(i0, length(i0), replace = TRUE),
             sample(i1, length(i1), replace = TRUE)))
  }

  # Cluster bootstrap: resample cluster ids within arm. A cluster whose units
  # span both arms is assigned to the arm holding most of its units.
  arm <- vapply(split(d, cl), function(x) as.integer(mean(x) >= 0.5),
                integer(1))
  ids <- names(arm)
  rows <- split(seq_along(d), cl)

  out <- integer(0)
  for (a in c(0L, 1L)) {
    pool <- ids[arm == a]
    if (length(pool) < 1L) return(NULL)
    picked <- sample(pool, length(pool), replace = TRUE)
    out <- c(out, unlist(rows[picked], use.names = FALSE))
  }
  out
}


#' Permute treatment assignment, optionally at the cluster level
#'
#' Internal helper for \code{\link{balance_prognosis_test}}. Holds the number of
#' treated units (or treated clusters) fixed.
#'
#' @param d Integer 0/1 treatment vector.
#' @param cl Character vector of cluster ids, or \code{NULL}.
#'
#' @return An integer 0/1 vector, or \code{NULL} if degenerate.
#' @keywords internal
#' @noRd
.bpt_permute <- function(d, cl) {

  if (is.null(cl)) return(sample(d))

  arm <- vapply(split(d, cl), function(x) as.integer(mean(x) >= 0.5),
                integer(1))
  if (length(arm) < 2L) return(NULL)
  perm <- sample(arm)
  names(perm) <- names(arm)
  as.integer(perm[as.character(cl)])
}
