#' Dose-Response Estimation for Continuous Treatments
#'
#' Estimates the dose-response function (average potential outcome as a
#' function of treatment dose) for continuous treatments using a
#' generalized propensity score (GPS) approach or regression-based
#' methods. Produces smooth dose-response curves with confidence bands.
#'
#' @param data A data frame.
#' @param outcome Character. Outcome variable name.
#' @param treatment Character. Continuous treatment variable name.
#' @param covariates Character vector. Covariate names for GPS model.
#' @param method Character. Estimation method: \code{"gps"} (generalized
#'   propensity score, default), \code{"regression"} (polynomial OLS),
#'   or \code{"loess"} (nonparametric LOESS).
#' @param n_grid Integer. Number of grid points for the dose-response curve.
#'   Default \code{50}.
#' @param poly_degree Integer. Polynomial degree for GPS or regression
#'   methods. Default \code{2}.
#' @param conf_level Numeric. Confidence level. Default \code{0.95}.
#' @param boot_reps Integer. Bootstrap replicates for CI. Default \code{200}.
#' @param seed Integer. Random seed. Default \code{42}.
#' @param trim_percentile Numeric. Trim extreme treatment values at this
#'   percentile (both tails). Default \code{0.01}.
#' @param plot Logical. Return a dose-response plot. Default \code{TRUE}.
#'
#' @return A list with:
#'   \describe{
#'     \item{dose_response}{Data frame: dose grid, estimated outcome,
#'       SE, CI bounds.}
#'     \item{ape}{Numeric. Average partial effect (slope at mean dose).}
#'     \item{plot}{ggplot2 dose-response curve.}
#'   }
#'
#' @references
#' Hirano, K., & Imbens, G. W. (2004). The propensity score with
#' continuous treatments. In A. Gelman & X.-L. Meng (Eds.),
#' \emph{Applied Bayesian Modeling and Causal Inference from Incomplete-
#' Data Perspectives} (pp. 73–84). Wiley.
#'
#' @examples
#' set.seed(42)
#' n <- 500
#' df <- data.frame(
#'   X1 = rnorm(n), X2 = rnorm(n),
#'   D  = runif(n, 0, 10)
#' )
#' df$D  <- df$D + df$X1  # confounded dose
#' df$Y  <- 0.5 * df$D - 0.03 * df$D^2 + df$X1 + rnorm(n)
#'
#' result <- dose_response(
#'   data       = df,
#'   outcome    = "Y",
#'   treatment  = "D",
#'   covariates = c("X1", "X2"),
#'   method     = "gps"
#' )
#' result$plot
#'
#' @importFrom stats lm dnorm predict quantile sd as.formula
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_rug
#'   labs theme_minimal
#' @export
dose_response <- function(data,
                           outcome,
                           treatment,
                           covariates     = NULL,
                           method         = c("gps", "regression", "loess"),
                           n_grid         = 50,
                           poly_degree    = 2,
                           conf_level     = 0.95,
                           boot_reps      = 200,
                           seed           = 42,
                           trim_percentile = 0.01,
                           plot           = TRUE) {

  method <- match.arg(method)

  req_cols <- c(outcome, treatment, covariates)
  missing  <- setdiff(req_cols, names(data))
  if (length(missing) > 0) {
    stop("Columns not found: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  Y <- data[[outcome]]
  D <- data[[treatment]]
  n <- length(Y)

  # Trim extremes
  d_lo <- stats::quantile(D, trim_percentile, na.rm = TRUE)
  d_hi <- stats::quantile(D, 1 - trim_percentile, na.rm = TRUE)
  keep <- D >= d_lo & D <= d_hi
  data <- data[keep, ]
  Y <- Y[keep]; D <- D[keep]; n <- length(Y)

  d_grid <- seq(d_lo, d_hi, length.out = n_grid)

  # --- Estimation ---
  estimate_dr <- function(dat, d_g) {
    Y_d <- dat[[outcome]]
    D_d <- dat[[treatment]]

    if (method == "gps") {
      # Step 1: estimate GPS via linear model for D ~ X
      if (!is.null(covariates)) {
        gps_form <- stats::as.formula(
          paste(treatment, "~", paste(covariates, collapse = " + "))
        )
        gps_mod  <- stats::lm(gps_form, data = dat)
        mu_d     <- stats::predict(gps_mod)
        sig_d    <- summary(gps_mod)$sigma
      } else {
        mu_d  <- rep(mean(D_d, na.rm = TRUE), n)
        sig_d <- stats::sd(D_d, na.rm = TRUE)
      }
      # GPS: f(D | X) ~ Normal
      gps <- stats::dnorm(D_d, mean = mu_d, sd = sig_d)

      # Step 2: outcome model on D and GPS
      out_form <- stats::as.formula(
        paste(outcome, "~ poly(", treatment, ",", poly_degree,
              ") + poly(gps_val,", poly_degree, ")")
      )
      dat2 <- dat
      dat2$gps_val <- gps
      out_mod <- stats::lm(out_form, data = dat2)

      # Step 3: dose-response at each grid point
      vapply(d_g, function(d_t) {
        # GPS at this dose for each unit
        gps_t <- stats::dnorm(d_t, mean = mu_d, sd = sig_d)
        new_d <- setNames(data.frame(
          matrix(rep(c(d_t, mean(gps_t)), 1), nrow = 1)
        ), c(treatment, "gps_val"))
        mean(stats::predict(out_mod, newdata = setNames(
          data.frame(d = rep(d_t, n), gps_val = gps_t),
          c(treatment, "gps_val")
        )), na.rm = TRUE)
      }, numeric(1))

    } else if (method == "regression") {
      # Polynomial regression of Y on D (and optionally X)
      rhs <- paste0("poly(", treatment, ",", poly_degree, ")")
      if (!is.null(covariates)) {
        rhs <- paste(rhs, "+", paste(covariates, collapse = " + "))
      }
      out_form <- stats::as.formula(paste(outcome, "~", rhs))
      out_mod  <- stats::lm(out_form, data = dat)

      # Marginal dose-response: set covariates at their means
      newdat <- data.frame(setNames(list(d_g), treatment))
      if (!is.null(covariates)) {
        for (v in covariates) {
          newdat[[v]] <- mean(dat[[v]], na.rm = TRUE)
        }
      }
      stats::predict(out_mod, newdata = newdat)

    } else {
      # LOESS
      loess_mod <- stats::loess(
        stats::as.formula(paste(outcome, "~", treatment)),
        data = dat, span = 0.5
      )
      stats::predict(loess_mod,
                     newdata = data.frame(setNames(list(d_g), treatment)))
    }
  }

  # Point estimates
  dr_vals <- estimate_dr(data, d_grid)

  # --- Bootstrap CI ---
  set.seed(seed)
  boot_dr <- matrix(NA, nrow = boot_reps, ncol = n_grid)
  for (b in seq_len(boot_reps)) {
    idx <- sample(n, replace = TRUE)
    boot_dr[b, ] <- tryCatch(
      estimate_dr(data[idx, , drop = FALSE], d_grid),
      error = function(e) rep(NA, n_grid)
    )
  }

  alpha   <- 1 - conf_level
  ci_lo   <- apply(boot_dr, 2, function(x) stats::quantile(x, alpha / 2, na.rm = TRUE))
  ci_hi   <- apply(boot_dr, 2, function(x) stats::quantile(x, 1 - alpha / 2, na.rm = TRUE))
  se_boot <- apply(boot_dr, 2, function(x) stats::sd(x, na.rm = TRUE))

  dr_df <- data.frame(
    dose     = d_grid,
    estimate = dr_vals,
    se       = se_boot,
    ci_lower = ci_lo,
    ci_upper = ci_hi
  )

  # Average partial effect: numerical derivative at mean dose
  d_mean <- mean(D, na.rm = TRUE)
  d_idx  <- which.min(abs(d_grid - d_mean))
  if (d_idx > 1 && d_idx < n_grid) {
    ape <- (dr_vals[d_idx + 1] - dr_vals[d_idx - 1]) /
           (d_grid[d_idx + 1] - d_grid[d_idx - 1])
  } else {
    ape <- NA
  }

  out <- list(
    dose_response = dr_df,
    ape           = ape,
    method        = method,
    plot          = NULL
  )

  if (plot) {
    p <- ggplot2::ggplot(dr_df, ggplot2::aes(x = dose, y = estimate)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
                           fill = "#4393C3", alpha = 0.25) +
      ggplot2::geom_line(color = "#2166AC", linewidth = 1.2) +
      ggplot2::geom_rug(
        data = data.frame(d = D),
        ggplot2::aes(x = d), sides = "b", alpha = 0.3,
        inherit.aes = FALSE
      ) +
      ggplot2::labs(
        x     = treatment,
        y     = outcome,
        title = "Dose-Response Function",
        subtitle = sprintf(
          "Method: %s | APE at mean dose = %.4f | %d%% CI",
          method, if (!is.na(ape)) ape else NA,
          round(conf_level * 100)
        )
      ) +
      ggplot2::theme_minimal(base_size = 12)

    out$plot <- p
  }

  invisible(out)
}
