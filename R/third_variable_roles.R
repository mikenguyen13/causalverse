# Third-variable roles in randomized experiments: is Z a moderator, a mediator,
# both, or neither?
#
# shift_test()        exact randomization test that treatment left Z unchanged
# equivalence_shift() TOST equivalence test on the standardized shift in Z
# role_test()         gated decision procedure returning a role verdict
# role_bounds()       wrapper around the 'prepost' bounds of Blackwell et al.
# simulate_role_data() / role_simulation()  data-generating worlds and a Monte
#                     Carlo comparison of decision rules

# ---------------------------------------------------------------------------
# internal helpers
# ---------------------------------------------------------------------------

.cv_binary_treat <- function(treat, arg = "treat") {
  if (is.logical(treat)) treat <- as.integer(treat)
  if (is.factor(treat) || is.character(treat)) {
    lev <- unique(stats::na.omit(as.character(treat)))
    if (length(lev) != 2L) {
      stop("`", arg, "` must have exactly two levels.", call. = FALSE)
    }
    lev <- sort(lev)
    treat <- as.integer(as.character(treat) == lev[2])
  }
  vals <- unique(stats::na.omit(treat))
  if (!all(vals %in% c(0, 1)) || length(vals) != 2L) {
    stop("`", arg, "` must be binary, coded 0/1 (or logical, or a two-level factor).",
         call. = FALSE)
  }
  as.integer(treat)
}

.cv_check_z <- function(z, treat, z_pre = NULL) {
  if (!is.numeric(z)) stop("`z` must be numeric.", call. = FALSE)
  if (length(z) != length(treat)) {
    stop("`z` and `treat` must have the same length.", call. = FALSE)
  }
  if (!is.null(z_pre)) {
    if (!is.numeric(z_pre)) stop("`z_pre` must be numeric.", call. = FALSE)
    if (length(z_pre) != length(z)) {
      stop("`z_pre` must have the same length as `z`.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.cv_check_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !(alpha > 0 && alpha < 0.5)) {
    stop("`alpha` must be a single number in (0, 0.5).", call. = FALSE)
  }
  invisible(TRUE)
}

.cv_check_B <- function(B) {
  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  as.integer(B)
}

# a permutation p-value can never be smaller than 1 / (B + 1)
.cv_warn_B_alpha <- function(B, alpha) {
  if (1 / (B + 1) >= alpha) {
    warning("With B = ", B, " permutations the smallest attainable p-value is ",
            format(signif(1 / (B + 1), 3)), ", which is not below alpha = ", alpha,
            ", so the randomization test can never reject. Increase `B` to at least ",
            ceiling(1 / alpha), ".", call. = FALSE)
  }
  invisible(TRUE)
}

# HC2 robust p-value and estimate for one coefficient of an lm fit
.cv_hc2 <- function(fit, term) {
  X <- stats::model.matrix(fit)
  e <- stats::residuals(fit)
  XtX_inv <- solve(crossprod(X))
  h <- rowSums((X %*% XtX_inv) * X)
  meat <- crossprod(X * (e / sqrt(1 - h)))
  V <- XtX_inv %*% meat %*% XtX_inv
  j <- match(term, colnames(X))
  if (is.na(j)) stop("Term `", term, "` not found in the model.", call. = FALSE)
  est <- unname(stats::coef(fit)[j])
  se <- sqrt(V[j, j])
  c(estimate = est, std.error = se, p.value = 2 * stats::pnorm(-abs(est / se)))
}

# permutation statistics for a block of label permutations (columns of P)
.cv_shift_stats <- function(z, P, o, at, n1, n0, which) {
  out <- list()
  if (any(c("mean", "var") %in% which)) {
    m1 <- colSums(P * z) / n1
    m0 <- colSums((1 - P) * z) / n0
    if ("mean" %in% which) out$mean <- abs(m1 - m0)
    if ("var" %in% which) {
      v1 <- colSums(P * z^2) / n1 - m1^2
      v0 <- colSums((1 - P) * z^2) / n0 - m0^2
      v1 <- pmax(v1, .Machine$double.eps); v0 <- pmax(v0, .Machine$double.eps)
      out$var <- abs(log(v1 / v0))
    }
  }
  if ("ks" %in% which) {
    Ps <- P[o, , drop = FALSE]
    F1 <- apply(Ps, 2, cumsum)[at, , drop = FALSE] / n1
    F0 <- apply(1 - Ps, 2, cumsum)[at, , drop = FALSE] / n0
    out$ks <- apply(abs(F1 - F0), 2, max)
  }
  out
}

# ---------------------------------------------------------------------------
# shift_test
# ---------------------------------------------------------------------------

#' Randomization Test That Treatment Left a Variable Unchanged
#'
#' Tests the sharp null hypothesis that a randomized binary treatment had no
#' effect on a variable \eqn{Z} for any unit, \eqn{Z_i(1) = Z_i(0)}. Under
#' complete randomization the test is exact: the observed statistic is compared
#' with its distribution over random re-assignments of the treatment labels.
#'
#' The default statistic combines three components, each sensitive to a
#' different kind of departure: the absolute difference in means, the absolute
#' log ratio of variances, and the Kolmogorov-Smirnov distance between the two
#' empirical distribution functions. The combined statistic is the minimum of
#' the three permutation p-values, and it is calibrated against the same
#' permutations, so the overall test keeps its size. Including the variance and
#' distribution components matters because a treatment can move \eqn{Z} up for
#' some units and down for others, leaving the mean unchanged; a mean test
#' alone misses such cancelling effects.
#'
#' If a baseline (pre-treatment) measure of \eqn{Z} is supplied through
#' \code{z_pre}, the test is applied to the residual of \code{z} after
#' regressing it on \code{z_pre}. Because \code{z_pre} is fixed before
#' assignment and the regression does not use the treatment, these residuals
#' are fixed under the sharp null, so the test remains exact and typically
#' gains a great deal of power.
#'
#' Ties (discrete or Likert-type variables) are handled by evaluating the
#' empirical distribution functions only at the distinct values. Units with a
#' missing value of \code{z}, \code{treat}, or \code{z_pre} are dropped.
#'
#' A non-significant result is not evidence that the treatment left \eqn{Z}
#' unchanged; use \code{\link{equivalence_shift}} for that claim.
#'
#' @param z Numeric vector, the candidate third variable measured after
#'   treatment.
#' @param treat Binary treatment (0/1, logical, or a two-level factor).
#' @param z_pre Optional numeric vector, the same variable measured before
#'   treatment.
#' @param stats Character vector, any of \code{"mean"}, \code{"var"},
#'   \code{"ks"}. Default all three.
#' @param B Integer, number of random permutations. Default \code{1999}.
#' @param block Integer, permutations processed per block (controls memory).
#'   Default \code{250}.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return An object of class \code{"shift_test"}: a list with the combined
#'   \code{p.value}, the component permutation p-values \code{p.components},
#'   the observed component statistics \code{statistic}, the group sizes
#'   \code{n1} and \code{n0}, group means and standard deviations, \code{B},
#'   and a \code{method} description.
#'
#' @references
#' Montgomery, J. M., Nyhan, B., & Torres, M. (2018). How conditioning on
#' posttreatment variables can ruin your experiment and what to do about it.
#' \emph{American Journal of Political Science}, 62(3), 760-775.
#'
#' Blackwell, M., Brown, J. R., Hill, S., Imai, K., & Yamamoto, T. (2025).
#' Priming bias versus post-treatment bias in experimental designs.
#' \emph{Political Analysis}, 33(4), 361-377.
#'
#' @seealso \code{\link{equivalence_shift}}, \code{\link{role_test}}
#'
#' @examples
#' set.seed(1)
#' treat <- rep(0:1, each = 200)
#' # treatment moves z up for half the units and down for the others
#' s <- sample(c(-1, 1), 400, replace = TRUE)
#' z <- rnorm(400) + 0.8 * treat * s
#' shift_test(z, treat, B = 499, seed = 2)
#' @export
shift_test <- function(z, treat, z_pre = NULL, stats = c("mean", "var", "ks"),
                       B = 1999, block = 250, seed = NULL) {
  stats <- match.arg(stats, c("mean", "var", "ks"), several.ok = TRUE)
  treat <- .cv_binary_treat(treat)
  .cv_check_z(z, treat, z_pre)
  B <- .cv_check_B(B)
  if (!is.numeric(block) || length(block) != 1L || block < 1) {
    stop("`block` must be a positive integer.", call. = FALSE)
  }
  block <- as.integer(block)
  ok <- !is.na(z) & !is.na(treat)
  if (!is.null(z_pre)) ok <- ok & !is.na(z_pre)
  z <- z[ok]; x <- treat[ok]
  n1 <- sum(x); n0 <- length(x) - n1
  if (n1 < 2 || n0 < 2) stop("Need at least two units in each treatment arm.", call. = FALSE)
  desc <- list(mean1 = mean(z[x == 1]), mean0 = mean(z[x == 0]),
               sd1 = stats::sd(z[x == 1]), sd0 = stats::sd(z[x == 0]))
  if (!is.null(z_pre)) {
    zp <- z_pre[ok]
    z <- stats::residuals(stats::lm(z ~ zp))
  }
  if (!is.null(seed)) set.seed(seed)

  o <- order(z)
  at <- which(!duplicated(z[o], fromLast = TRUE))
  obs <- .cv_shift_stats(z, matrix(x, ncol = 1), o, at, n1, n0, stats)
  perm <- lapply(stats, function(s) numeric(0)); names(perm) <- stats
  done <- 0L
  while (done < B) {
    k <- min(block, B - done)
    P <- vapply(seq_len(k), function(i) sample(x), numeric(length(x)))
    if (k == 1L) P <- matrix(P, ncol = 1)
    st <- .cv_shift_stats(z, P, o, at, n1, n0, stats)
    for (s in stats) perm[[s]] <- c(perm[[s]], st[[s]])
    done <- done + k
  }
  # permutation p-value of every column (observed first) for each component
  all_p <- vapply(stats, function(s) {
    v <- c(obs[[s]], perm[[s]])
    rank(-v, ties.method = "max") / (B + 1)
  }, numeric(B + 1))
  all_p <- matrix(all_p, nrow = B + 1)
  minp <- apply(all_p, 1, min)
  p <- mean(minp <= minp[1])

  structure(list(
    p.value = p,
    p.components = stats::setNames(all_p[1, ], stats),
    statistic = unlist(obs),
    n1 = n1, n0 = n0, desc = desc, B = B,
    residualized = !is.null(z_pre),
    method = paste0("Randomization test of no effect on Z for any unit (",
                    paste(stats, collapse = ", "), "; min-p combination",
                    if (!is.null(z_pre)) ", residualized on baseline Z" else "", ")")
  ), class = "shift_test")
}

#' @export
print.shift_test <- function(x, digits = 3, ...) {
  cat("\n", x$method, "\n\n", sep = "")
  cat(sprintf("  Treated: n = %d, mean = %.*f, sd = %.*f\n", x$n1, digits, x$desc$mean1, digits, x$desc$sd1))
  cat(sprintf("  Control: n = %d, mean = %.*f, sd = %.*f\n", x$n0, digits, x$desc$mean0, digits, x$desc$sd0))
  cat("  Component p-values: ",
      paste(sprintf("%s = %.*f", names(x$p.components), digits, x$p.components), collapse = ", "), "\n", sep = "")
  cat(sprintf("  Combined p-value = %.*f  (B = %d permutations)\n\n", digits, x$p.value, x$B))
  invisible(x)
}

# ---------------------------------------------------------------------------
# equivalence_shift
# ---------------------------------------------------------------------------

#' Equivalence Test That Treatment Did Not Shift a Variable
#'
#' Two one-sided tests (TOST) of whether the average effect of a randomized
#' treatment on \eqn{Z}, in standard-deviation units of \eqn{Z}, lies inside
#' \eqn{(-\delta, \delta)}. Rejecting both one-sided nulls supports the claim
#' that any shift is smaller than the margin. If \code{z_pre} is supplied the
#' effect is estimated by regression adjustment on the baseline measure, which
#' sharpens the test. Standard errors are HC2 heteroskedasticity robust.
#'
#' @param z Numeric vector measured after treatment.
#' @param treat Binary treatment.
#' @param z_pre Optional baseline measure of \code{z}.
#' @param margin Equivalence margin \eqn{\delta} in standard deviations of
#'   \code{z}. Default \code{0.2}. Choose it before seeing the data.
#' @param alpha Significance level of each one-sided test. Default \code{0.05}.
#'
#' @return A list of class \code{"equivalence_shift"} with the standardized
#'   estimate, its standard error, the \eqn{1 - 2\alpha} confidence interval,
#'   the TOST p-value, and the logical \code{equivalent}.
#'
#' @references
#' Hartman, E., & Hidalgo, F. D. (2018). An equivalence approach to balance
#' and placebo tests. \emph{American Journal of Political Science}, 62(4),
#' 1000-1013.
#'
#' @seealso \code{\link{shift_test}}, \code{\link{role_test}}
#'
#' @examples
#' set.seed(1)
#' treat <- rep(0:1, each = 500)
#' z <- rnorm(1000)
#' equivalence_shift(z, treat, margin = 0.2)
#' @export
equivalence_shift <- function(z, treat, z_pre = NULL, margin = 0.2, alpha = 0.05) {
  treat <- .cv_binary_treat(treat)
  .cv_check_z(z, treat, z_pre)
  if (!is.numeric(margin) || length(margin) != 1L || !(margin > 0)) {
    stop("`margin` must be a single positive number.", call. = FALSE)
  }
  .cv_check_alpha(alpha)
  ok <- !is.na(z) & !is.na(treat)
  if (!is.null(z_pre)) ok <- ok & !is.na(z_pre)
  d <- data.frame(z = z[ok], X = treat[ok])
  if (!is.null(z_pre)) d$zp <- z_pre[ok]
  if (sum(d$X) < 2 || sum(1 - d$X) < 2) {
    stop("Need at least two units in each treatment arm.", call. = FALSE)
  }
  sdz <- stats::sd(d$z)
  if (!is.finite(sdz) || sdz == 0) stop("`z` has no variation.", call. = FALSE)
  fit <- if (is.null(z_pre)) stats::lm(z ~ X, data = d) else stats::lm(z ~ X + zp, data = d)
  r <- .cv_hc2(fit, "X")
  est <- r[["estimate"]] / sdz; se <- r[["std.error"]] / sdz
  zc <- stats::qnorm(1 - alpha)
  p_tost <- max(stats::pnorm((est - margin) / se), 1 - stats::pnorm((est + margin) / se))
  structure(list(
    estimate = est, std.error = se,
    conf.int = c(est - zc * se, est + zc * se),
    margin = margin, alpha = alpha, p.value = p_tost,
    equivalent = (est - zc * se > -margin) && (est + zc * se < margin)
  ), class = "equivalence_shift")
}

#' @export
print.equivalence_shift <- function(x, digits = 3, ...) {
  cat(sprintf("\nEquivalence test (TOST) of the standardized shift in Z, margin +/- %.2f SD\n", x$margin))
  cat(sprintf("  Estimate = %.*f SD, %d%% CI [%.*f, %.*f], TOST p = %.*f\n",
              digits, x$estimate, round(100 * (1 - 2 * x$alpha)), digits, x$conf.int[1],
              digits, x$conf.int[2], digits, x$p.value))
  cat("  Equivalence", if (x$equivalent) "established.\n\n" else "NOT established.\n\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# role_test
# ---------------------------------------------------------------------------

#' Is a Third Variable a Moderator, a Mediator, Both, or Neither?
#'
#' A gated decision procedure for a randomized binary treatment \eqn{X}, an
#' outcome \eqn{Y}, and a candidate third variable \eqn{Z}. It operationalizes
#' the MacArthur criteria of Kraemer et al. (2002, 2008): a moderator precedes
#' treatment and is unaffected by it; a mediator is affected by treatment and
#' in turn related to the outcome.
#'
#' \strong{With a baseline measure} (\code{z_pre} supplied):
#' \enumerate{
#'   \item \code{\link{shift_test}} on \code{z_post} residualized on
#'     \code{z_pre}: did treatment move \eqn{Z}?
#'   \item Moderation is tested on the baseline measure, \eqn{X \times
#'     Z_{pre}}, which is a valid effect-modification test regardless of
#'     step 1.
#'   \item If step 1 rejects, the \eqn{Z \to Y} path is tested by regressing
#'     \eqn{Y} on \eqn{X}, \eqn{Z_{post}}, and \eqn{Z_{pre}}.
#'   \item If step 1 does not reject, \code{\link{equivalence_shift}} decides
#'     between "neither" and "undetermined" when there is no moderation.
#' }
#' Verdicts: \code{"both"}, \code{"mediator"}, \code{"moderator"},
#' \code{"neither"}, \code{"post-treatment, no Z-Y path"}, \code{"undetermined"}.
#'
#' \strong{Post-only design} (\code{z_pre = NULL}): if step 1 rejects,
#' \eqn{Z} cannot be a moderator and the verdict is \code{"mediator"} or
#' \code{"post-treatment, no Z-Y path"}. If it does not reject, the
#' equivalence test must pass before the \eqn{X \times Z_{post}} interaction
#' is interpreted as moderation; otherwise the verdict is
#' \code{"undetermined"}. Moderation by a variable the treatment has moved
#' cannot be detected in this design, so "both" is never returned.
#'
#' What is and is not identified: the effect of \eqn{X} on \eqn{Z} and
#' moderation by baseline \eqn{Z} are identified by randomization. The
#' \eqn{Z \to Y} path is not; a \code{"mediator"} verdict therefore means
#' "mediator candidate" and rests on sequential ignorability. Pair it with a
#' sensitivity analysis (for example \code{\link{mediation_analysis}}) or a
#' design that manipulates \eqn{Z}.
#'
#' @param data A data frame.
#' @param treat,outcome,z_post Character names of the binary treatment, the
#'   outcome, and the post-treatment measure of the third variable.
#' @param z_pre Optional character name of the baseline measure.
#' @param margin Equivalence margin in SD units for \code{\link{equivalence_shift}}.
#'   Default \code{0.2}.
#' @param alpha Significance level for every step. Default \code{0.05}.
#' @param B Permutations for \code{\link{shift_test}}. Default \code{1999}.
#' @param seed Optional seed.
#'
#' @return An object of class \code{"role_test"}: a list with \code{verdict},
#'   \code{design} (\code{"pre-post"} or \code{"post-only"}), a data frame
#'   \code{steps} summarizing each test, and the component objects
#'   \code{shift} and \code{equivalence}.
#'
#' @references
#' Kraemer, H. C., Wilson, G. T., Fairburn, C. G., & Agras, W. S. (2002).
#' Mediators and moderators of treatment effects in randomized clinical
#' trials. \emph{Archives of General Psychiatry}, 59(10), 877-883.
#'
#' Kraemer, H. C., Kiernan, M., Essex, M., & Kupfer, D. J. (2008). How and why
#' criteria defining moderators and mediators differ between the Baron &
#' Kenny and MacArthur approaches. \emph{Health Psychology}, 27(2, Suppl.),
#' S101-S108.
#'
#' @seealso \code{\link{shift_test}}, \code{\link{equivalence_shift}},
#'   \code{\link{role_bounds}}, \code{\link{simulate_role_data}}
#'
#' @examples
#' d <- simulate_role_data("both", n = 1000, seed = 3)
#' role_test(d, treat = "X", outcome = "Y", z_post = "Z_post", z_pre = "Z_pre",
#'           B = 499, seed = 4)
#' @export
role_test <- function(data, treat, outcome, z_post, z_pre = NULL,
                      margin = 0.2, alpha = 0.05, B = 1999, seed = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  .cv_check_alpha(alpha)
  B <- .cv_check_B(B)
  .cv_warn_B_alpha(B, alpha)
  vars <- c(treat, outcome, z_post, z_pre)
  miss <- setdiff(vars, names(data))
  if (length(miss)) stop("Not found in `data`: ", paste(miss, collapse = ", "), call. = FALSE)
  d <- data.frame(X = .cv_binary_treat(data[[treat]], "treat"),
                  Y = data[[outcome]], Zpost = data[[z_post]])
  if (!is.null(z_pre)) d$Zpre <- data[[z_pre]]
  d <- d[stats::complete.cases(d), , drop = FALSE]

  sh <- shift_test(d$Zpost, d$X, z_pre = if (!is.null(z_pre)) d$Zpre, B = B, seed = seed)
  eq <- equivalence_shift(d$Zpost, d$X, z_pre = if (!is.null(z_pre)) d$Zpre,
                          margin = margin, alpha = alpha)
  affected <- sh$p.value < alpha

  if (!is.null(z_pre)) {
    mod <- .cv_hc2(stats::lm(Y ~ X * Zpre, data = d), "X:Zpre")
    bpath <- .cv_hc2(stats::lm(Y ~ X + Zpost + Zpre, data = d), "Zpost")
    is_mod <- mod[["p.value"]] < alpha; is_b <- bpath[["p.value"]] < alpha
    verdict <- if (affected) {
      if (is_b && is_mod) "both" else if (is_b) "mediator" else
        if (is_mod) "moderator" else "post-treatment, no Z-Y path"
    } else if (is_mod) "moderator" else if (eq$equivalent) "neither" else "undetermined"
    mod_label <- "Moderation: X x Z_pre interaction"
    b_label <- "Z -> Y path: Z_post | X, Z_pre"
  } else {
    mod <- .cv_hc2(stats::lm(Y ~ X * Zpost, data = d), "X:Zpost")
    bpath <- .cv_hc2(stats::lm(Y ~ X + Zpost, data = d), "Zpost")
    is_mod <- mod[["p.value"]] < alpha; is_b <- bpath[["p.value"]] < alpha
    verdict <- if (affected) {
      if (is_b) "mediator" else "post-treatment, no Z-Y path"
    } else if (eq$equivalent) {
      if (is_mod) "moderator" else "neither"
    } else "undetermined"
    mod_label <- "Moderation: X x Z_post interaction (gated)"
    b_label <- "Z -> Y path: Z_post | X"
  }

  steps <- data.frame(
    step = c("Treatment moved Z (randomization test)",
             sprintf("Z unaffected within +/- %.2f SD (TOST)", margin),
             mod_label, b_label),
    estimate = c(NA_real_, eq$estimate, mod[["estimate"]], bpath[["estimate"]]),
    p.value = c(sh$p.value, eq$p.value, mod[["p.value"]], bpath[["p.value"]]),
    result = c(if (affected) "yes" else "not rejected",
               if (eq$equivalent) "equivalent" else "not established",
               if (is_mod) "significant" else "not significant",
               if (is_b) "significant" else "not significant"),
    stringsAsFactors = FALSE)

  structure(list(verdict = verdict,
                 design = if (is.null(z_pre)) "post-only" else "pre-post",
                 steps = steps, shift = sh, equivalence = eq,
                 n = nrow(d), alpha = alpha),
            class = "role_test")
}

#' @export
print.role_test <- function(x, digits = 3, ...) {
  cat("\nThird-variable role test (", x$design, " design, n = ", x$n, ")\n\n", sep = "")
  s <- x$steps
  s$estimate <- ifelse(is.na(s$estimate), "", formatC(s$estimate, digits = digits, format = "f"))
  s$p.value <- formatC(s$p.value, digits = digits, format = "f")
  print(s, row.names = FALSE, right = FALSE)
  cat("\nVerdict:", x$verdict, "\n")
  if (x$verdict %in% c("mediator", "both")) {
    cat("Note: the Z -> Y path is not randomized; 'mediator' means mediator candidate\n",
        "under sequential ignorability. Add a sensitivity analysis.\n", sep = "")
  }
  if (x$design == "post-only") {
    cat("Note: post-only design cannot detect moderation by a variable the treatment moved.\n")
  }
  cat("\n")
  invisible(x)
}

# ---------------------------------------------------------------------------
# role_bounds
# ---------------------------------------------------------------------------

#' Bounds on a Treatment-by-Moderator Interaction Under Priming or Post-Treatment Bias
#'
#' A thin wrapper around the \pkg{prepost} package, which implements the
#' nonparametric bounds of Blackwell, Brown, Hill, Imai, and Yamamoto (2025)
#' for the interaction between a randomized treatment and a binary moderator
#' when the moderator is measured after treatment (post-treatment bias) or
#' before it (priming bias). The outcome must be binary.
#'
#' @param data A data frame.
#' @param treat,outcome,moderator Character names of the binary treatment,
#'   binary outcome, and binary moderator.
#' @param design \code{"post"} (moderator measured after treatment) or
#'   \code{"pre"} (measured before).
#' @param sensitivity Logical. If \code{TRUE}, return the sensitivity analysis
#'   over the share of affected respondents (\code{gamma} for post,
#'   \code{theta} for pre) instead of the sharp bounds.
#' @param ... Further arguments passed to the \pkg{prepost} function, for
#'   example \code{sims}, \code{g_by}, \code{g_max}, \code{t_by},
#'   \code{moderator_mono}, or \code{stable_mod}.
#'
#' @return The object returned by \code{prepost::post_bounds},
#'   \code{prepost::pre_bounds}, \code{prepost::post_sens}, or
#'   \code{prepost::pre_sens}.
#'
#' @references
#' Blackwell, M., Brown, J. R., Hill, S., Imai, K., & Yamamoto, T. (2025).
#' Priming bias versus post-treatment bias in experimental designs.
#' \emph{Political Analysis}, 33(4), 361-377.
#'
#' @examples
#' \dontrun{
#' data(land_experiment, package = "prepost")
#' post <- subset(land_experiment, prepost == 1)
#' role_bounds(post, "treat_comb", "support", "land_insecure", design = "post", sims = 200)
#' }
#' @export
role_bounds <- function(data, treat, outcome, moderator, design = c("post", "pre"),
                        sensitivity = FALSE, ...) {
  design <- match.arg(design)
  if (!requireNamespace("prepost", quietly = TRUE)) {
    stop("role_bounds() needs the 'prepost' package: install.packages('prepost')", call. = FALSE)
  }
  f <- stats::as.formula(paste(outcome, "~", treat))
  m <- stats::as.formula(paste("~", moderator))
  fun <- switch(paste(design, sensitivity),
                "post FALSE" = "post_bounds", "pre FALSE" = "pre_bounds",
                "post TRUE" = "post_sens", "pre TRUE" = "pre_sens")
  args <- list(formula = f, data = data, moderator = m, ...)
  if (fun %in% c("post_bounds", "post_sens") && is.null(args$progress)) args$progress <- FALSE
  if (fun == "post_sens" && is.null(args$g_by)) args$g_by <- 0.01
  if (fun == "pre_sens" && is.null(args$t_by)) args$t_by <- 0.01
  do.call(utils::getFromNamespace(fun, "prepost"), args)
}

# ---------------------------------------------------------------------------
# simulation tools
# ---------------------------------------------------------------------------

#' Simulate Experiments With a Known Third-Variable Role
#'
#' Generates a randomized experiment with a binary treatment \code{X}, a
#' third variable measured before (\code{Z_pre}) and after (\code{Z_post})
#' treatment with measurement error, and an outcome \code{Y}, in one of six
#' worlds whose true role is known.
#'
#' @details
#' The six worlds:
#' \describe{
#'   \item{\code{"neither"}}{Treatment does not move \eqn{Z}; \eqn{Z} affects
#'     \eqn{Y} but does not modify the effect of \eqn{X}.}
#'   \item{\code{"moderator"}}{Baseline \eqn{Z} modifies the effect of \eqn{X};
#'     treatment does not move \eqn{Z}.}
#'   \item{\code{"mediator"}}{Treatment moves \eqn{Z}, which affects \eqn{Y}
#'     linearly.}
#'   \item{\code{"curved_mediator"}}{As \code{"mediator"} with a convex
#'     \eqn{Z \to Y} effect, which makes baseline \eqn{Z} a genuine effect
#'     modifier: the true role is "both".}
#'   \item{\code{"cancelling"}}{Treatment moves \eqn{Z} up for some units and
#'     down for others, the units it moves up benefit more, and baseline
#'     \eqn{Z} modifies nothing: not a moderator.}
#'   \item{\code{"both"}}{Treatment moves \eqn{Z} and baseline \eqn{Z}
#'     modifies the effect.}
#' }
#'
#' @param world One of the six worlds above.
#' @param n Sample size (even). Treatment is completely randomized, n/2 per arm.
#' @param me Standard deviation of the measurement error in each wave of
#'   \eqn{Z}. Default \code{0.3}.
#' @param seed Optional seed.
#'
#' @return A data frame with columns \code{X}, \code{Z_pre}, \code{Z_post},
#'   \code{Y}, and attributes \code{world} and \code{truth}.
#'
#' @seealso \code{\link{role_test}}, \code{\link{role_simulation}}
#'
#' @examples
#' head(simulate_role_data("cancelling", n = 200, seed = 1))
#' @export
simulate_role_data <- function(world = c("neither", "moderator", "mediator",
                                         "curved_mediator", "cancelling", "both"),
                               n = 400, me = 0.3, seed = NULL) {
  world <- match.arg(world)
  if (!is.null(seed)) set.seed(seed)
  X <- sample(rep(0:1, length.out = n))
  Z0 <- stats::rnorm(n); e <- stats::rnorm(n)
  S <- sample(c(-1, 1), n, replace = TRUE)
  U <- 0.5 * stats::rnorm(n)
  switch(world,
    neither   = { Z1 <- Z0 + U;           Y <- 0.5 * X + 0.5 * Z1 + e },
    moderator = { Z1 <- Z0 + U;           Y <- 0.5 * X + 0.5 * Z0 + 0.4 * X * Z0 + e },
    mediator  = { Z1 <- Z0 + U + 0.6 * X; Y <- 0.3 * X + 0.5 * Z1 + e },
    curved_mediator = { Z1 <- Z0 + U + 0.6 * X
                        Y <- 0.3 * X + 0.5 * Z1 + 0.25 * Z1^2 + e },
    cancelling = { Z1 <- Z0 + U + 0.6 * X * S
                   Y <- 0.4 * X + 0.4 * S * X + 0.5 * Z1 + e },
    both      = { Z1 <- Z0 + U + 0.6 * X; Y <- 0.3 * X + 0.5 * Z1 + 0.4 * X * Z0 + e })
  truth <- c(neither = "neither", moderator = "moderator", mediator = "mediator",
             curved_mediator = "both", cancelling = "mediator", both = "both")[[world]]
  out <- data.frame(X = X, Z_pre = Z0 + me * stats::rnorm(n),
                    Z_post = Z1 + me * stats::rnorm(n), Y = Y)
  attr(out, "world") <- world
  attr(out, "truth") <- truth
  out
}

# one replication of the three decision rules (internal; used by role_simulation)
.cv_role_one <- function(world, n, B, margin, alpha) {
  d <- simulate_role_data(world, n = n)
  p_int <- .cv_hc2(stats::lm(Y ~ X * Z_post, data = d), "X:Z_post")[["p.value"]]
  p_a <- .cv_hc2(stats::lm(Z_post ~ X, data = d), "X")[["p.value"]]
  p_b <- .cv_hc2(stats::lm(Y ~ X + Z_post, data = d), "Z_post")[["p.value"]]
  naive <- if (p_int < alpha) "moderator" else
    if (p_a < alpha && p_b < alpha) "mediator" else "neither"
  post <- role_test(d, "X", "Y", "Z_post", margin = margin, alpha = alpha, B = B)$verdict
  pp <- role_test(d, "X", "Y", "Z_post", z_pre = "Z_pre", margin = margin,
                  alpha = alpha, B = B)$verdict
  c(naive = naive, post_only = post, pre_post = pp)
}

#' Monte Carlo Comparison of Rules for Classifying a Third Variable
#'
#' Repeatedly simulates experiments with \code{\link{simulate_role_data}} and
#' applies three decision rules to each: the naive rule that reads a
#' significant \eqn{X \times Z_{post}} interaction as moderation (and
#' otherwise checks joint significance of the \eqn{X \to Z} and
#' \eqn{Z \to Y} paths), \code{\link{role_test}} in a post-only design, and
#' \code{\link{role_test}} with a baseline measure.
#'
#' @param worlds Character vector of worlds. Default all six.
#' @param n Sample size per simulated experiment. Default \code{400}.
#' @param reps Replications per world. Default \code{1000}.
#' @param B Permutations per randomization test. Default \code{499}.
#' @param margin,alpha Passed to \code{\link{role_test}}. Defaults
#'   \code{0.25} and \code{0.05}.
#' @param cores Number of worker processes (uses \pkg{parallel}). Default
#'   \code{1}.
#' @param seed Seed for reproducibility (L'Ecuyer streams when
#'   \code{cores > 1}).
#'
#' @return A data frame of class \code{"role_simulation"} with one row per
#'   replication and rule (\code{world}, \code{truth}, \code{rep}, \code{rule},
#'   \code{verdict}). Its \code{summary()} method returns the share of correct
#'   verdicts, moderator verdicts, and undetermined verdicts by world and rule.
#'
#' @seealso \code{\link{role_test}}, \code{\link{simulate_role_data}}
#'
#' @examples
#' \donttest{
#' sims <- role_simulation(worlds = c("moderator", "cancelling"), reps = 20, B = 199)
#' summary(sims)
#' }
#' @export
role_simulation <- function(worlds = c("neither", "moderator", "mediator",
                                       "curved_mediator", "cancelling", "both"),
                            n = 400, reps = 1000, B = 499, margin = 0.25,
                            alpha = 0.05, cores = 1, seed = NULL) {
  worlds <- match.arg(worlds, several.ok = TRUE)
  .cv_check_alpha(alpha)
  B <- .cv_check_B(B)
  .cv_warn_B_alpha(B, alpha)
  if (!is.numeric(reps) || length(reps) != 1L || reps < 1) {
    stop("`reps` must be a positive integer.", call. = FALSE)
  }
  reps <- as.integer(reps)
  cl <- NULL
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, requireNamespace("causalverse", quietly = TRUE))
    if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)
  } else if (!is.null(seed)) {
    set.seed(seed)
  }
  # worker defined with a bare environment so that nothing large is serialized
  worker <- function(i, w, n, B, margin, alpha) {
    get(".cv_role_one", envir = asNamespace("causalverse"))(w, n, B, margin, alpha)
  }
  environment(worker) <- baseenv()
  run_world <- function(w) {
    res <- if (!is.null(cl)) {
      parallel::parSapply(cl, seq_len(reps), worker, w = w, n = n, B = B,
                          margin = margin, alpha = alpha)
    } else {
      vapply(seq_len(reps), function(i) .cv_role_one(w, n, B, margin, alpha), character(3))
    }
    data.frame(world = w, truth = attr(simulate_role_data(w, n = 4), "truth"),
               rep = rep(seq_len(reps), each = 3), rule = rep(rownames(res), reps),
               verdict = as.vector(res), stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, lapply(worlds, run_world))
  class(out) <- c("role_simulation", "data.frame")
  out
}

#' @export
summary.role_simulation <- function(object, ...) {
  d <- as.data.frame(unclass(object), stringsAsFactors = FALSE)
  keys <- unique(d[, c("world", "truth", "rule")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    v <- d$verdict[d$world == keys$world[i] & d$rule == keys$rule[i]]
    data.frame(world = keys$world[i], truth = keys$truth[i], rule = keys$rule[i],
               pct_correct = 100 * mean(v == keys$truth[i]),
               pct_moderator = 100 * mean(v == "moderator"),
               pct_undetermined = 100 * mean(v == "undetermined"),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
