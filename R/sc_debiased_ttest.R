#' Debiased Synthetic Control Estimation with a Self-Normalized t-Test
#'
#' Implements the K-fold cross-fitting bias correction and self-normalized
#' t-test for the average treatment effect on the treated (ATT) in synthetic
#' control designs, following Chernozhukov, Wuthrich, and Zhu (2026).
#'
#' The canonical synthetic control (SC) estimator is biased whenever the
#' pre-treatment fit is imperfect, and its sampling distribution is difficult
#' to approximate because it depends on the long-run variance of a serially
#' correlated gap process. This function addresses both problems at once:
#' cross-fitting removes the bias, and self-normalization sidesteps long-run
#' variance estimation entirely by studentizing with the dispersion of the
#' fold-specific estimates.
#'
#' @section Setup and notation:
#' Let unit \eqn{i = 0} be the single treated unit and \eqn{i = 1, \dots, N}
#' the untreated donors. Periods \eqn{1, \dots, T_0} are pre-treatment and
#' \eqn{T_0 + 1, \dots, T} are post-treatment, with \eqn{T_1 = T - T_0}.
#' Writing \eqn{Y_{it}(0)} and \eqn{Y_{it}(1)} for the potential outcomes and
#' \eqn{\alpha_{0t} = Y_{0t}(1) - Y_{0t}(0)} for the treatment effect, the
#' target parameter is the ATT over the post-treatment window,
#' \deqn{\tau = \frac{1}{T_1} \sum_{t = T_0 + 1}^{T} \alpha_{0t}.}
#'
#' @section The estimator:
#' Set \eqn{r = \min(\lfloor T_0 / K \rfloor, T_1)} and partition the
#' pre-treatment period into \eqn{K} consecutive blocks
#' \eqn{H_k = \{(k-1)r + 1, \dots, kr\}}, with
#' \eqn{H_{-k} = \{1, \dots, T_0\} \setminus H_k}. For each fold \eqn{k}:
#'
#' \enumerate{
#'   \item Estimate SC weights on the held-out pre-treatment periods,
#'     \deqn{\hat{w}^{(k)} \in \arg\min_{w \in \mathcal{W}}
#'       \sum_{t \in H_{-k}} \Big( Y_{0t} - \sum_{i=1}^{N} w_i Y_{it} \Big)^2,}
#'     over the simplex \eqn{\mathcal{W} = \{w : w_i \ge 0, \sum_i w_i = 1\}}.
#'   \item Form the fold-specific debiased estimate as the post-treatment gap
#'     minus the gap on the held-out block,
#'     \deqn{\hat{\tau}_k = \frac{1}{T_1} \sum_{t = T_0+1}^{T}
#'       \Big( Y_{0t} - \sum_i \hat{w}^{(k)}_i Y_{it} \Big)
#'       - \frac{1}{|H_k|} \sum_{t \in H_k}
#'       \Big( Y_{0t} - \sum_i \hat{w}^{(k)}_i Y_{it} \Big).}
#' }
#'
#' The point estimate averages the folds,
#' \eqn{\hat{\tau} = K^{-1} \sum_{k=1}^{K} \hat{\tau}_k}. Because
#' \eqn{\hat{w}^{(k)}} is estimated without using \eqn{H_k}, the second term is
#' an out-of-sample estimate of the pre-treatment gap and therefore removes the
#' bias of the first term rather than mechanically absorbing it.
#'
#' @section Inference:
#' Inference is self-normalized: the fold estimates \eqn{\hat{\tau}_k} supply
#' their own dispersion, so no long-run variance is estimated. With
#' \deqn{\hat{\sigma}_{\hat\tau} = \sqrt{1 + \frac{Kr}{T_1}} \cdot
#'   \sqrt{\frac{1}{K-1} \sum_{k=1}^{K} (\hat{\tau}_k - \hat{\tau})^2},}
#' the statistic
#' \eqn{\mathbb{T}_K = \sqrt{K}(\hat{\tau} - \tau) / \hat{\sigma}_{\hat\tau}}
#' is asymptotically pivotal with a \eqn{t_{K-1}} reference distribution, and
#' \deqn{\hat{\tau} \pm t_{K-1}(1 - \alpha/2) \cdot
#'   \hat{\sigma}_{\hat\tau} / \sqrt{K}}
#' is a valid \eqn{1 - \alpha} confidence interval. The factor
#' \eqn{\sqrt{1 + Kr/T_1}} corrects for the dependence the shared
#' post-treatment window induces across the fold estimates.
#'
#' @section Assumptions:
#' Validity requires (i) \eqn{\ell_2}-consistency of the fold weights for a
#' pseudo-true weight vector, (ii) weak dependence (beta-mixing) of the
#' outcome process with bounded moments, and (iii) either covariance
#' stationarity of \eqn{Y_t(0)}, or a common-nonstationarity structure in
#' which any nonstationary component is shared across all units. The test does
#' \emph{not} require the SC weights to be consistent for a true set of
#' weights, nor a perfect pre-treatment fit; this is the sense in which it is
#' robust to misspecification. It does require \eqn{T_1} small relative to
#' \eqn{T_0}.
#'
#' @param Y Numeric matrix of outcomes with units in rows and time periods in
#'   columns, following the \code{synthdid::panel.matrices()} convention: the
#'   first \code{N0} rows are control units and the remaining rows are treated
#'   units. Exactly one treated unit is required.
#' @param N0 Integer. Number of control units (the first \code{N0} rows of
#'   \code{Y}).
#' @param T0 Integer. Number of pre-treatment periods (the first \code{T0}
#'   columns of \code{Y}).
#' @param K Integer. Number of cross-fitting folds. Must be at least 2, since
#'   the reference distribution has \code{K - 1} degrees of freedom. Default
#'   \code{4}. Larger \code{K} gives more degrees of freedom and shorter
#'   intervals but leaves fewer periods per block; the method is designed for
#'   small \code{K}.
#' @param alpha Numeric. Significance level for the confidence interval.
#'   Default \code{0.05}.
#' @param tol Numeric. Convergence tolerance for the simplex-constrained
#'   least-squares solver. Default \code{1e-8}.
#' @param max_iter Integer. Maximum iterations for the solver. Default
#'   \code{10000}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{\code{att}}{The debiased ATT estimate \eqn{\hat{\tau}}.}
#'     \item{\code{se}}{The self-normalized standard error
#'       \eqn{\hat{\sigma}_{\hat\tau} / \sqrt{K}}.}
#'     \item{\code{t_stat}}{The t-statistic for \eqn{H_0: \tau = 0}.}
#'     \item{\code{p_value}}{Two-sided p-value from the \eqn{t_{K-1}}
#'       distribution.}
#'     \item{\code{ci}}{Named numeric vector with the lower and upper
#'       confidence bounds.}
#'     \item{\code{df}}{Degrees of freedom, \code{K - 1}.}
#'     \item{\code{tau_folds}}{The \code{K} fold-specific estimates
#'       \eqn{\hat{\tau}_k}.}
#'     \item{\code{weights}}{An \code{N0} by \code{K} matrix of fold-specific
#'       SC weights.}
#'     \item{\code{att_naive}}{The undebiased SC estimate using weights fit on
#'       all \code{T0} pre-treatment periods, for comparison.}
#'     \item{\code{K}, \code{r}, \code{alpha}}{The tuning values used.}
#'   }
#'
#' @references
#' Chernozhukov, V., Wuthrich, K., & Zhu, Y. (2026). Debiasing and t-Tests for
#' Synthetic Control Inference on Average Causal Effects.
#' \emph{Journal of Political Economy}. \doi{10.1086/742424}
#'
#' Abadie, A., Diamond, A., & Hainmueller, J. (2010). Synthetic control methods
#' for comparative case studies: Estimating the effect of California's tobacco
#' control program. \emph{Journal of the American Statistical Association},
#' 105(490), 493-505.
#'
#' @seealso \code{\link{synthdid_se_placebo}} and
#'   \code{\link{synthdid_se_jacknife}} for the placebo and jackknife
#'   alternatives; \code{\link{sc_gap_plot}} for visualizing the gap process.
#'
#' @importFrom stats sd pt qt
#' @export
#' @examples
#' # Simulated panel with a known ATT of 2, a factor structure the donors
#' # can reproduce, and serially correlated noise.
#' set.seed(42)
#' N0 <- 12; T0 <- 40; T1 <- 8; Tt <- T0 + T1
#' f <- cumsum(rnorm(Tt))                       # common nonstationary factor
#' load_c <- runif(N0, 0.5, 1.5)
#' Yc <- outer(load_c, f) + matrix(rnorm(N0 * Tt, sd = 0.3), N0, Tt)
#' y1 <- mean(load_c) * f + rnorm(Tt, sd = 0.3)
#' y1[(T0 + 1):Tt] <- y1[(T0 + 1):Tt] + 2       # true ATT = 2
#' Y <- rbind(Yc, y1)
#'
#' fit <- sc_debiased_ttest(Y, N0 = N0, T0 = T0, K = 4)
#' fit$att
#' fit$ci
#' fit$p_value
sc_debiased_ttest <- function(Y,
                              N0,
                              T0,
                              K = 4,
                              alpha = 0.05,
                              tol = 1e-8,
                              max_iter = 10000) {

  # ---- Input validation -----------------------------------------------------
  if (!is.matrix(Y) || !is.numeric(Y)) {
    stop("`Y` must be a numeric matrix with units in rows and periods in columns.")
  }
  if (anyNA(Y)) {
    stop("`Y` must not contain missing values.")
  }

  N <- nrow(Y)
  Tt <- ncol(Y)

  if (length(N0) != 1L || !is.finite(N0) || N0 != as.integer(N0)) {
    stop("`N0` must be a single integer.")
  }
  if (length(T0) != 1L || !is.finite(T0) || T0 != as.integer(T0)) {
    stop("`T0` must be a single integer.")
  }
  N0 <- as.integer(N0)
  T0 <- as.integer(T0)

  if (N0 < 2L || N0 >= N) {
    stop("`N0` must be at least 2 and strictly less than nrow(Y).")
  }
  if (N - N0 != 1L) {
    stop(
      "sc_debiased_ttest() is defined for a single treated unit, but nrow(Y) - N0 = ",
      N - N0,
      ". Aggregate the treated units, or apply the test to one treated unit at a time."
    )
  }
  if (T0 < 2L || T0 >= Tt) {
    stop("`T0` must be at least 2 and strictly less than ncol(Y).")
  }

  if (length(K) != 1L || !is.finite(K) || K != as.integer(K) || K < 2L) {
    stop("`K` must be a single integer >= 2; the t-distribution needs K - 1 >= 1 degrees of freedom.")
  }
  K <- as.integer(K)

  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number strictly between 0 and 1.")
  }

  T1 <- Tt - T0
  r <- min(T0 %/% K, T1)

  if (r < 1L) {
    stop(
      "Cross-fitting blocks are empty: floor(T0 / K) = ", T0 %/% K,
      " and T1 = ", T1, ", so r = ", r,
      ". Reduce `K` (needs T0 >= K) or use a design with more pre-treatment periods."
    )
  }

  # ---- Split the panel ------------------------------------------------------
  Y_ctrl <- Y[seq_len(N0), , drop = FALSE]   # N0 x Tt
  y_trt  <- as.numeric(Y[N0 + 1L, ])         # length Tt

  post <- (T0 + 1L):Tt

  # ---- Cross-fitting --------------------------------------------------------
  tau_folds <- numeric(K)
  W <- matrix(NA_real_, nrow = N0, ncol = K)

  for (k in seq_len(K)) {
    H_k  <- ((k - 1L) * r + 1L):(k * r)      # held-out block
    H_mk <- setdiff(seq_len(T0), H_k)        # training periods

    w_k <- sc_simplex_ls(
      X = t(Y_ctrl[, H_mk, drop = FALSE]),   # |H_mk| x N0
      y = y_trt[H_mk],
      tol = tol,
      max_iter = max_iter
    )
    W[, k] <- w_k

    gap_post <- mean(y_trt[post] - as.numeric(crossprod(Y_ctrl[, post, drop = FALSE], w_k)))
    gap_hk   <- mean(y_trt[H_k] - as.numeric(crossprod(Y_ctrl[, H_k, drop = FALSE], w_k)))

    tau_folds[k] <- gap_post - gap_hk
  }

  # ---- Point estimate and self-normalized inference -------------------------
  att <- mean(tau_folds)

  sigma_hat <- sqrt(1 + (K * r) / T1) * stats::sd(tau_folds)
  se <- sigma_hat / sqrt(K)

  df <- K - 1L

  # The self-normalized statistic degenerates when the fold estimates carry no
  # dispersion, which happens with an (essentially) exact pre-treatment fit.
  # The standard error is then numerically zero rather than genuinely small, so
  # reporting a huge t-statistic would overstate precision. Flag it instead.
  #
  # Dispersion at the level of the solver's own convergence noise carries no
  # information, so the threshold is tied to `tol` and the outcome scale rather
  # than to machine epsilon: below roughly 100x the solver tolerance the spread
  # across folds is numerical, not statistical.
  se_tol <- max(1e-6, 100 * tol) * max(1, stats::sd(y_trt))
  degenerate <- !is.finite(se) || se <= se_tol

  if (degenerate) {
    warning(
      "The K fold estimates are numerically identical, so the self-normalized ",
      "standard error is zero and the t-test is not defined. This usually means ",
      "the pre-treatment fit is exact (often a donor pool that reproduces the ",
      "treated unit). Point estimates are still returned; treat the inference as ",
      "unavailable rather than infinitely precise.",
      call. = FALSE
    )
  }

  t_stat <- if (degenerate) NA_real_ else att / se
  p_value <- if (degenerate) NA_real_ else 2 * stats::pt(-abs(t_stat), df = df)
  crit <- stats::qt(1 - alpha / 2, df = df)

  ci <- c(lower = att - crit * se, upper = att + crit * se)

  # ---- Undebiased benchmark, for comparison ---------------------------------
  w_full <- sc_simplex_ls(
    X = t(Y_ctrl[, seq_len(T0), drop = FALSE]),
    y = y_trt[seq_len(T0)],
    tol = tol,
    max_iter = max_iter
  )
  att_naive <- mean(y_trt[post] - as.numeric(crossprod(Y_ctrl[, post, drop = FALSE], w_full)))

  list(
    att        = att,
    se         = se,
    t_stat     = t_stat,
    p_value    = p_value,
    ci         = ci,
    df         = df,
    tau_folds  = tau_folds,
    weights    = W,
    att_naive  = att_naive,
    K          = K,
    r          = r,
    alpha      = alpha
  )
}


#' Simplex-Constrained Least Squares via Projected Gradient Descent
#'
#' Solves \eqn{\min_w \|y - Xw\|_2^2} subject to \eqn{w_i \ge 0} and
#' \eqn{\sum_i w_i = 1}. Used internally to fit canonical synthetic control
#' weights without introducing a quadratic-programming dependency.
#'
#' @param X Numeric matrix, observations in rows and donors in columns.
#' @param y Numeric response vector.
#' @param tol Convergence tolerance on the change in the weight vector.
#' @param max_iter Maximum number of iterations.
#'
#' @return A numeric vector of weights on the unit simplex.
#' @keywords internal
#' @noRd
sc_simplex_ls <- function(X, y, tol = 1e-8, max_iter = 10000) {
  p <- ncol(X)
  w <- rep(1 / p, p)

  XtX <- crossprod(X)
  Xty <- crossprod(X, y)

  # Step size 1 / L with L the Lipschitz constant of the gradient, 2 * lambda_max(X'X).
  lambda_max <- max(abs(eigen(XtX, symmetric = TRUE, only.values = TRUE)$values))
  if (!is.finite(lambda_max) || lambda_max <= 0) {
    return(w)
  }
  step <- 1 / (2 * lambda_max)

  # Accelerated projected gradient (FISTA). The Nesterov momentum term turns the
  # O(1/i) convergence of plain projected gradient into O(1/i^2), which matters
  # here because the solver is called K + 1 times per estimate and repeatedly
  # inside simulations.
  z <- w
  theta <- 1

  for (i in seq_len(max_iter)) {
    grad <- 2 * (XtX %*% z - Xty)
    w_new <- sc_project_simplex(as.numeric(z - step * grad))

    theta_new <- (1 + sqrt(1 + 4 * theta^2)) / 2
    z <- w_new + ((theta - 1) / theta_new) * (w_new - w)

    converged <- max(abs(w_new - w)) < tol
    w <- w_new
    theta <- theta_new
    if (converged) break
  }
  w
}


#' Euclidean Projection onto the Unit Simplex
#'
#' Projects a vector onto \eqn{\{w : w_i \ge 0, \sum_i w_i = 1\}} using the
#' sorting algorithm of Duchi et al. (2008).
#'
#' @param v Numeric vector to project.
#'
#' @return A numeric vector on the unit simplex.
#' @keywords internal
#' @noRd
sc_project_simplex <- function(v) {
  n <- length(v)
  if (n == 1L) {
    return(1)
  }
  u <- sort(v, decreasing = TRUE)
  cssv <- cumsum(u) - 1
  ind <- seq_len(n)
  cond <- (u - cssv / ind) > 0
  rho <- if (any(cond)) max(ind[cond]) else 1L
  theta <- cssv[rho] / rho
  pmax(v - theta, 0)
}
