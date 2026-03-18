#' Monte Carlo Power Simulation for Causal Designs
#'
#' Estimates statistical power (or sample requirements) for a variety of
#' causal inference designs via Monte Carlo simulation. Unlike closed-form
#' formulas, this approach handles non-normal outcomes, clustering, and
#' complex treatment assignment mechanisms.
#'
#' @param dgp A function \code{function(n, effect_size, ...)} that generates
#'   one simulated dataset as a data frame with columns \code{Y} (outcome) and
#'   \code{W} (treatment). If \code{NULL}, a default DGP is used (linear model
#'   with Gaussian noise).
#' @param estimator A function \code{function(data)} that takes the simulated
#'   data frame and returns a named list with at least
#'   \code{estimate} (numeric) and \code{p_value} (numeric). If \code{NULL},
#'   a simple OLS t-test on W is used.
#' @param n_seq Integer vector. Sample sizes to evaluate. Default
#'   \code{c(50, 100, 200, 500)}.
#' @param effect_seq Numeric vector. Effect sizes (standardized) to evaluate.
#'   Default \code{c(0.1, 0.2, 0.3, 0.5)}.
#' @param n_sims Integer. Number of Monte Carlo replications per cell.
#'   Default \code{1000}.
#' @param alpha Numeric. Significance level. Default \code{0.05}.
#' @param seed Integer. Random seed. Default \code{1}.
#' @param n_cores Integer. Number of cores for parallel simulation. Default
#'   \code{1}.
#' @param plot Logical. Produce power curves. Default \code{TRUE}.
#'
#' @return A named list with two elements: \code{results} (a data frame with
#'   columns \code{n}, \code{effect_size}, \code{power}, \code{mean_estimate},
#'   \code{bias}, \code{rmse}, and \code{coverage}) and \code{plot} (a ggplot2
#'   power curve, or \code{NULL} when \code{plot = FALSE}).
#'
#' @examples
#' \dontrun{
#' # Default DGP: linear Gaussian model
#' ps <- power_sim(
#'   n_seq      = c(100, 200, 400),
#'   effect_seq = c(0.2, 0.3, 0.5),
#'   n_sims     = 500
#' )
#' ps$results
#' ps$plot
#'
#' # Custom DGP: clustered data
#' cluster_dgp <- function(n, effect_size) {
#'   n_clusters  <- max(10, n %/% 20)
#'   cluster_ids <- rep(seq_len(n_clusters), each = ceiling(n / n_clusters))[seq_len(n)]
#'   cluster_re  <- rnorm(n_clusters, 0, 0.5)
#'   W  <- as.integer(cluster_ids <= n_clusters / 2)
#'   Y  <- effect_size * W + cluster_re[cluster_ids] + rnorm(n)
#'   data.frame(Y = Y, W = W, cluster = cluster_ids)
#' }
#' cluster_est <- function(data) {
#'   fit <- lm(Y ~ W, data = data)
#'   sm  <- summary(fit)$coefficients["W", ]
#'   list(estimate = sm[1], p_value = sm[4])
#' }
#' ps_cl <- power_sim(dgp = cluster_dgp, estimator = cluster_est,
#'                    n_seq = c(200, 500), n_sims = 500)
#' }
#'
#' @export
power_sim <- function(dgp       = NULL,
                      estimator = NULL,
                      n_seq      = c(50, 100, 200, 500),
                      effect_seq = c(0.1, 0.2, 0.3, 0.5),
                      n_sims     = 1000,
                      alpha      = 0.05,
                      seed       = 1,
                      n_cores    = 1,
                      plot       = TRUE) {

  set.seed(seed)

  # Default DGP: linear Gaussian
  if (is.null(dgp)) {
    dgp <- function(n, effect_size) {
      W <- stats::rbinom(n, 1, 0.5)
      Y <- effect_size * W + stats::rnorm(n)
      data.frame(Y = Y, W = W)
    }
  }

  # Default estimator: OLS t-test
  if (is.null(estimator)) {
    estimator <- function(data) {
      fit <- lm(Y ~ W, data = data)
      sm  <- summary(fit)$coefficients
      if (!"W" %in% rownames(sm)) return(list(estimate = NA, p_value = NA))
      list(
        estimate = sm["W", 1],
        se       = sm["W", 2],
        p_value  = sm["W", 4]
      )
    }
  }

  run_cell <- function(n, effect_size) {
    ests   <- numeric(n_sims)
    pvals  <- numeric(n_sims)
    ses    <- numeric(n_sims)
    for (s in seq_len(n_sims)) {
      dat <- tryCatch(dgp(n, effect_size), error = function(e) NULL)
      if (is.null(dat)) {
        ests[s]  <- NA
        pvals[s] <- NA
        ses[s]   <- NA
        next
      }
      res <- tryCatch(estimator(dat), error = function(e) list(estimate=NA, p_value=NA, se=NA))
      ests[s]  <- if (!is.null(res$estimate)) res$estimate else NA
      pvals[s] <- if (!is.null(res$p_value)) res$p_value else NA
      ses[s]   <- if (!is.null(res$se)) res$se else NA
    }
    power    <- mean(pvals < alpha, na.rm = TRUE)
    mean_est <- mean(ests, na.rm = TRUE)
    bias     <- mean_est - effect_size
    rmse     <- sqrt(mean((ests - effect_size)^2, na.rm = TRUE))
    ci_lo    <- ests - qnorm(1 - alpha / 2) * ses
    ci_hi    <- ests + qnorm(1 - alpha / 2) * ses
    coverage <- mean(ci_lo <= effect_size & ci_hi >= effect_size, na.rm = TRUE)

    data.frame(
      n           = n,
      effect_size = effect_size,
      power       = power,
      mean_estimate = mean_est,
      bias        = bias,
      rmse        = rmse,
      coverage    = coverage
    )
  }

  grid <- expand.grid(n = n_seq, effect_size = effect_seq)

  if (n_cores > 1 && requireNamespace("parallel", quietly = TRUE)) {
    rows <- parallel::mcmapply(
      function(n, e) run_cell(n, e),
      grid$n, grid$effect_size,
      SIMPLIFY = FALSE, mc.cores = n_cores
    )
  } else {
    rows <- mapply(
      function(n, e) run_cell(n, e),
      grid$n, grid$effect_size,
      SIMPLIFY = FALSE
    )
  }

  results <- do.call(rbind, rows)
  results$effect_label <- paste0("d = ", results$effect_size)

  out <- list(results = results, plot = NULL)

  if (plot) {
    p <- ggplot2::ggplot(results,
        ggplot2::aes(x = n, y = power,
                     color = factor(effect_size),
                     group = factor(effect_size))) +
      ggplot2::geom_hline(yintercept = 0.8, linetype = "dashed",
                          color = "grey40", linewidth = 0.7) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(0, 1, 0.2)
      ) +
      ggplot2::scale_x_continuous(breaks = n_seq) +
      ggplot2::annotate("text", x = min(n_seq), y = 0.82,
                        label = "80% power", hjust = 0, size = 3,
                        color = "grey40") +
      ggplot2::labs(
        title  = "Monte Carlo Power Simulation",
        x      = "Sample Size (N)",
        y      = "Statistical Power",
        color  = "Effect Size (d)"
      ) +
      causalverse::ama_theme()

    out$plot <- p
  }

  invisible(out)
}
