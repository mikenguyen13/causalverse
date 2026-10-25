#' Andrews-Kasy Pairwise U-Statistic for Publication Bias
#'
#' Estimates the relative publication probability \eqn{q} of statistically
#' insignificant IV estimates from the moment condition of Andrews and Kasy
#' (2019, online appendix), as used by Pancost and Schaller (2026). For each
#' pair of estimates, the more precise estimate is noised up to the standard
#' error of the less precise one; absent selection, the two are equally
#' likely to be significant. The weighted average difference is a quadratic
#' in \eqn{1/q}.
#'
#' @param b_iv Numeric vector of (normalized) IV estimates.
#' @param se_iv Their standard errors.
#' @param q Optional value of \eqn{q} at which to evaluate the U-statistic.
#' @param crit Critical value. Default 1.96.
#'
#' @return A list with \code{q} (estimate, or the supplied value),
#'   \code{U_q} (U-statistic at \code{q}), \code{U_1} (at \eqn{q = 1}, a test
#'   of no publication bias), and the quadratic coefficients.
#' @references Andrews, I., & Kasy, M. (2019). Identification of and
#'   correction for publication bias. \emph{American Economic Review},
#'   109(8), 2766-2794.
#' @family meta_me
#' @export
meta_me_ustat <- function(b_iv, se_iv, q = NULL, crit = 1.96) {
  ok <- is.finite(b_iv) & is.finite(se_iv)
  b <- b_iv[ok]; s <- se_iv[ok]
  N <- length(b)
  i1 <- rep.int(seq_len(N - 1), (N - 1):1)
  i2 <- unlist(lapply(2:N, function(k) k:N), use.names = FALSE)
  noisy <- ifelse(s[i1] >= s[i2], i1, i2)
  prec <- ifelse(s[i1] >= s[i2], i2, i1)
  z <- s[noisy]; zp <- s[prec]; bn <- b[noisy]; bp <- b[prec]
  ind1 <- as.numeric(abs(bn) < crit * z)
  sd_add <- sqrt(z^2 - zp^2)
  h <- ifelse(sd_add > 0,
              stats::pnorm((crit * z - bp) / sd_add) - stats::pnorm((-crit * z - bp) / sd_add),
              as.numeric(abs(bp) < crit * z))
  X <- ind1 - h
  t <- abs(bn) / z; tp <- abs(bp) / zp
  both <- t > crit & tp > crit
  none <- t < crit & tp < crit
  one <- xor(t > crit, tp > crit)
  a <- sum(X[none]); bb <- sum(X[one]); cc <- sum(X[both])
  if (is.null(q)) {
    disc <- bb^2 - 4 * a * cc
    roots <- if (disc >= 0) (-bb + c(1, -1) * sqrt(disc)) / (2 * a) else c(NA, NA)
    roots <- roots[is.finite(roots) & roots > 0]
    q <- if (length(roots)) 1 / roots[1] else NA_real_
  }
  list(q = q, U_q = (a / q^2 + bb / q + cc) / length(X), U_1 = mean(X),
       coef = c(a = a, b = bb, c = cc), n_pairs = length(X))
}

# Moments targeted by the structural model, in a fixed order.
.me_pb_targets <- function(b_iv, b_ols, z_iv, z_ols) {
  x <- b_iv - mean(b_iv); y <- b_ols - mean(b_ols)
  c(E_iv = mean(b_iv), sd_iv = stats::sd(b_iv),
    E_ols = mean(b_ols), sd_ols = stats::sd(b_ols),
    Ez_iv = mean(z_iv), sdz_iv = stats::sd(z_iv),
    Ez_ols = mean(z_ols), sdz_ols = stats::sd(z_ols),
    # constant OLS standard errors (e.g. after dividing by them) carry no
    # correlation information; treat them as degenerate lognormal
    corrz = if (stats::sd(z_ols) > 0 && stats::sd(z_iv) > 0) stats::cor(z_iv, z_ols) else 0,
    meta_ols = sum(x * y) / sum(x^2))
}

# Map a target vector to model parameters (exactly identified mapping).
.me_pb_map <- function(T, q, fix_theta = NULL) {
  s_iv <- sqrt(log(1 + T[["sdz_iv"]]^2 / T[["Ez_iv"]]^2))
  m_iv <- log(T[["Ez_iv"]]) - 0.5 * s_iv^2
  s_ols <- sqrt(log(1 + T[["sdz_ols"]]^2 / T[["Ez_ols"]]^2))
  m_ols <- log(T[["Ez_ols"]]) - 0.5 * s_ols^2
  covar <- T[["corrz"]] * T[["sdz_iv"]] * T[["sdz_ols"]]
  covz <- log(1 + covar / exp(m_iv + m_ols + 0.5 * (s_iv^2 + s_ols^2)))
  sb2 <- T[["sd_iv"]]^2 - (T[["sdz_iv"]]^2 + T[["Ez_iv"]]^2)
  sb <- sqrt(max(sb2, 1e-12))
  if (is.null(fix_theta)) {
    theta <- T[["meta_ols"]] * T[["sd_iv"]]^2 / sb^2
    theta <- if (theta > 1) 1 else if (theta < 0) 0.01 else theta
    sg2 <- T[["sd_ols"]]^2 - (T[["sdz_ols"]]^2 + T[["Ez_ols"]]^2) - sb^2 * theta^2
  } else {
    theta <- fix_theta
    sg2 <- 0
  }
  list(q = q, theta = theta, mu_beta = T[["E_iv"]], sigma_beta = sb,
       mu_gamma = T[["E_ols"]] - T[["E_iv"]] * theta,
       sigma_gamma = sqrt(max(sg2, 0)),
       mu_zeta_iv = m_iv, sigma_zeta_iv = s_iv,
       mu_zeta_ols = m_ols, sigma_zeta_ols = s_ols, cov_zeta = covz,
       feasible = sb2 > 0 && (sg2 >= 0 || !is.null(fix_theta)))
}

# Simulate J studies, apply selection on |t_IV|, return kept and all draws.
.me_pb_sim <- function(P, N, seed, var_theta = NULL, R2max = NULL, corrR2 = 0) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  e <- matrix(stats::rnorm(N * 10), N, 10)
  s1 <- P$sigma_zeta_iv; s2 <- P$sigma_zeta_ols
  c12 <- P$cov_zeta
  l21 <- if (s1 > 0) c12 / s1 else 0
  l22 <- sqrt(max(s2^2 - l21^2, 0))
  z_iv <- exp(P$mu_zeta_iv + s1 * e[, 8])
  z_ols <- exp(P$mu_zeta_ols + l21 * e[, 8] + l22 * e[, 9])
  th <- if (is.null(var_theta)) rep(P$theta, N) else P$theta + sqrt(var_theta) * e[, 6]
  w <- rep(1, N)
  if (!is.null(R2max)) {
    rz <- 2 * sin(pi * corrR2 / 6)
    U1 <- stats::pnorm(e[, 6]); U2 <- stats::pnorm(rz * e[, 6] + sqrt(1 - rz^2) * e[, 7])
    w <- sqrt((1 - R2max * U1) / (1 - R2max * U2)); w <- w / mean(w)
  }
  beta <- P$mu_beta + P$sigma_beta * e[, 1]
  gam <- P$mu_gamma + P$sigma_gamma * e[, 2]
  b_ols <- w * (th * beta + gam + z_ols * e[, 4])
  b_iv <- w * (beta + z_iv * e[, 5])
  z_ols <- w * z_ols; z_iv <- w * z_iv
  keep <- stats::pnorm(e[, 10]) < P$q | abs(b_iv) / z_iv > 1.96
  list(b_iv = b_iv[keep], b_ols = b_ols[keep], z_iv = z_iv[keep], z_ols = z_ols[keep],
       all_b_iv = b_iv, all_z_iv = z_iv, keep = keep)
}

.me_pb_solve <- function(T0, q, fix_theta, N, seed, max_iter, tol, step,
                         var_theta, R2max, corrR2, match) {
  Tc <- T0
  P <- .me_pb_map(Tc, q, fix_theta)
  crit <- Inf; it <- 0
  while (crit > tol && it < max_iter) {
    it <- it + 1
    S <- .me_pb_sim(P, N, seed, var_theta, R2max, corrR2)
    m <- .me_pb_targets(S$b_iv, S$b_ols, S$z_iv, S$z_ols)
    gap <- (T0 - m)[match]
    crit <- max(abs(gap))
    if (crit <= tol) break
    t <- step
    repeat {
      Tn <- Tc
      Tn[match] <- Tc[match] + t * gap
      Pn <- .me_pb_map(Tn, q, fix_theta)
      if (Pn$feasible || t < 1e-4) break
      t <- t / 2
    }
    Tc <- Tn; P <- Pn
  }
  S <- .me_pb_sim(P, N, seed, var_theta, R2max, corrR2)
  list(params = P, sims = S, model = .me_pb_targets(S$b_iv, S$b_ols, S$z_iv, S$z_ols),
       iterations = it, converged = crit <= tol)
}

#' Structural Model of Measurement Error and Publication Bias
#'
#' Simulates studies with true effects \eqn{\beta_j \sim N(\mu_\beta,
#' \sigma_\beta^2)}, omitted variable bias \eqn{\gamma_j \sim N(\mu_\gamma,
#' \sigma_\gamma^2)}, OLS limit \eqn{\theta\beta_j + \gamma_j}, jointly
#' lognormal standard errors, and selective publication that keeps
#' insignificant IV estimates with probability \eqn{q}. \eqn{q} is estimated
#' by the Andrews-Kasy U-statistic (\code{meta_me_ustat()}); the remaining
#' parameters are estimated by indirect inference, matching the means and
#' standard deviations of coefficients and standard errors, the correlation
#' of standard errors, and the uncorrected meta-OLS slope.
#'
#' @param data Output of \code{meta_me_prepare()}.
#' @param fix_q Fix \eqn{q} (use 1 for no publication bias). Default
#'   \code{NULL} estimates it.
#' @param fix_theta Fix \eqn{\theta} (use 1 for no measurement error; the
#'   OLS dispersion and meta-OLS moments are then not targeted and
#'   \eqn{\sigma_\gamma = 0}). Default \code{NULL} estimates it.
#' @param n_sim Number of simulated studies per replication. Default
#'   200,000. The original used one replication of 10,000, at which the
#'   estimate of \eqn{\theta} varies by about +/- 0.1 across seeds.
#' @param n_rep Number of independent simulation replications (seeds
#'   \code{seed}, \code{seed + 1}, ...). Parameters are averaged over
#'   converged replications and their spread is returned as \code{sim_sd}.
#'   Default 5.
#' @param seed Seed for common random numbers. Default 19850323.
#' @param n_boot Paper-level bootstrap replications for standard errors.
#'   Default 0.
#' @param max_iter,tol,step Fixed-point controls.
#' @param var_theta,R2max,corrR2 Optional heterogeneity in proxy quality and
#'   in normalization weights (used for normalization robustness checks).
#'
#' @return An object of class \code{meta_me_pubbias} with \code{params},
#'   \code{moments} (data vs model), \code{ustat}, simulated draws for the
#'   t-distribution figure, and bootstrap standard errors when requested.
#' @family meta_me
#' @export
meta_me_pubbias <- function(data, fix_q = NULL, fix_theta = NULL, n_sim = 2e5,
                            n_rep = 5, seed = 19850323, n_boot = 0,
                            max_iter = 300, tol = 1e-4, step = 0.5,
                            var_theta = NULL, R2max = NULL, corrR2 = 0) {
  d <- as.data.frame(data)
  d <- d[is.finite(d$b_iv) & is.finite(d$b_ols) & is.finite(d$se_iv) & is.finite(d$se_ols), ]
  pvec <- function(P) c(q = P$q, theta = P$theta, mu_beta = P$mu_beta,
                        sigma_beta = P$sigma_beta, mu_gamma = P$mu_gamma,
                        sigma_gamma = P$sigma_gamma, mu_zeta_iv = P$mu_zeta_iv,
                        sigma_zeta_iv = P$sigma_zeta_iv, mu_zeta_ols = P$mu_zeta_ols,
                        sigma_zeta_ols = P$sigma_zeta_ols,
                        rho_zeta = P$cov_zeta / (P$sigma_zeta_iv * P$sigma_zeta_ols))
  fit_one <- function(dd, reps = n_rep) {
    T0 <- .me_pb_targets(dd$b_iv, dd$b_ols, dd$se_iv, dd$se_ols)
    q <- if (is.null(fix_q)) meta_me_ustat(dd$b_iv, dd$se_iv)$q else fix_q
    match <- names(T0)
    if (!is.null(fix_theta)) match <- setdiff(match, c("sd_ols", "meta_ols"))
    runs <- lapply(seq_len(reps) - 1, function(k) {
      .me_pb_solve(T0, q, fix_theta, n_sim, seed + k, max_iter, tol, step,
                   var_theta, R2max, corrR2, match)
    })
    conv <- vapply(runs, function(r) r$converged, TRUE)
    use <- if (any(conv)) which(conv) else seq_along(runs)
    P <- do.call(rbind, lapply(runs[use], function(r) pvec(r$params)))
    list(params = colMeans(P),
         sim_sd = if (nrow(P) > 1) apply(P, 2, stats::sd) else P[1, ] * NA,
         first = runs[[use[1]]], data_moments = T0,
         n_converged = sum(conv), n_rep = reps)
  }
  r <- fit_one(d)
  pv <- r$params
  se <- rep(NA_real_, length(pv)); names(se) <- names(pv)
  if (n_boot > 0) {
    papers <- unique(d$paper)
    B <- matrix(NA_real_, n_boot, length(pv))
    for (b in seq_len(n_boot)) {
      draw <- sample(papers, length(papers), replace = TRUE)
      dd <- do.call(rbind, lapply(draw, function(p) d[d$paper == p, ]))
      rb <- tryCatch(fit_one(dd, reps = 1), error = function(e) NULL)
      if (!is.null(rb)) B[b, ] <- rb$params
    }
    se <- apply(B, 2, stats::sd, na.rm = TRUE); names(se) <- names(pv)
  }
  S <- r$first$sims
  u_data <- meta_me_ustat(d$b_iv, d$se_iv, q = pv[["q"]])
  ns <- seq_len(min(2000, length(S$b_iv)))
  u_model <- meta_me_ustat(S$b_iv[ns], S$z_iv[ns], q = pv[["q"]])
  structure(list(params = pv, se = se, sim_sd = r$sim_sd,
                 moments = data.frame(moment = names(r$data_moments),
                                      data = unname(r$data_moments),
                                      model = unname(r$first$model[names(r$data_moments)])),
                 ustat_data = u_data$U_q, ustat_model = u_model$U_q,
                 ustat_no_bias = u_data$U_1, sims = S,
                 converged = r$n_converged > 0, n_converged = r$n_converged,
                 n_rep = r$n_rep, fix_q = fix_q, fix_theta = fix_theta),
            class = "meta_me_pubbias")
}

#' @export
print.meta_me_pubbias <- function(x, ...) {
  cat("Structural model of measurement error and publication bias\n")
  cat(sprintf("  q (relative publication prob.) = %.4f, theta = %.3f (simulation SD %.3f, %d of %d replications converged)\n",
              x$params[["q"]], x$params[["theta"]], x$sim_sd[["theta"]],
              x$n_converged, x$n_rep))
  print(x$moments, digits = 3, row.names = FALSE)
  invisible(x)
}

#' Publication-Bias Model with and without Each Channel
#'
#' Re-estimates \code{meta_me_pubbias()} with both channels, without
#' publication bias (\eqn{q = 1}), without measurement error
#' (\eqn{\theta = 1}), and without either, and reports the implied meta-OLS
#' slope, U-statistic and OLS dispersion (Table 12 of Pancost and Schaller,
#' 2026).
#'
#' @inheritParams meta_me_pubbias
#' @param ... Passed to \code{meta_me_pubbias()}.
#' @return A data frame with one row per model.
#' @family meta_me
#' @export
meta_me_pubbias_variants <- function(data, ...) {
  specs <- list(Both = list(NULL, NULL), "No publication bias" = list(1, NULL),
                "No measurement error" = list(NULL, 1), Neither = list(1, 1))
  do.call(rbind, lapply(names(specs), function(nm) {
    f <- meta_me_pubbias(data, fix_q = specs[[nm]][[1]], fix_theta = specs[[nm]][[2]], ...)
    S <- f$sims
    x <- S$b_iv - mean(S$b_iv)
    data.frame(model = nm, theta = f$params[["theta"]], q = f$params[["q"]],
               meta_ols = sum(x * (S$b_ols - mean(S$b_ols))) / sum(x^2),
               ustat = f$ustat_data, sd_ols = stats::sd(S$b_ols))
  }))
}
