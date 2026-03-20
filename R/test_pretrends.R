#' Unified Pre-Trends Testing for Event Study Models
#'
#' Tests the parallel trends assumption by performing a joint F-test of
#' pre-treatment coefficients and optionally running sensitivity analysis
#' via HonestDiD. Accepts either a \code{fixest} model object or manually
#' supplied coefficient estimates and variance-covariance matrix.
#'
#' @param model A \code{fixest} model object from \code{\link[fixest]{feols}}.
#'   If \code{NULL}, \code{betahat} and \code{sigma} must be provided.
#' @param betahat Numeric vector of pre-treatment coefficient estimates.
#'   Used only when \code{model} is \code{NULL}.
#' @param sigma Variance-covariance matrix for \code{betahat}.
#'   Used only when \code{model} is \code{NULL}.
#' @param pre_periods Integer vector indicating which coefficients are
#'   pre-treatment. If \code{NULL} (default), auto-detected from negative
#'   period names in the \code{fixest} model (e.g., coefficients containing
#'   \code{"-1"}, \code{"-2"}, etc.).
#' @param conf_level Numeric. Confidence level for individual tests.
#'   Default is \code{0.95}.
#' @param honest_did Logical. If \code{TRUE} and the \pkg{HonestDiD} package
#'   is installed, run sensitivity analysis using
#'   \code{HonestDiD::createSensitivityResults}. Default is \code{FALSE}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{joint_test}{A list with \code{f_stat}, \code{df1}, \code{df2},
#'       and \code{p_value} from the joint F-test of pre-treatment
#'       coefficients equal to zero.}
#'     \item{individual_tests}{A data frame with columns \code{term},
#'       \code{estimate}, \code{std_error}, \code{t_stat}, \code{p_value},
#'       \code{ci_lower}, \code{ci_upper} for each pre-treatment coefficient.}
#'     \item{plot}{A \code{ggplot2} object showing pre-treatment coefficients
#'       with confidence intervals and a reference line at zero.}
#'     \item{honest_did}{If requested and available, results from HonestDiD
#'       sensitivity analysis. Otherwise \code{NULL}.}
#'   }
#'
#' @examples
#' \dontrun{
#' data(base_stagg, package = "fixest")
#' mod <- feols(y ~ i(time_to_treatment, ref = -1) | id + year,
#'              data = base_stagg)
#' result <- test_pretrends(mod)
#' result$joint_test
#' result$plot
#'
#' # Manual input
#' result2 <- test_pretrends(
#'   betahat = c(-0.02, 0.01, -0.03),
#'   sigma = diag(c(0.01, 0.01, 0.01)),
#'   pre_periods = 1:3
#' )
#' }
#'
#' @importFrom stats pf qnorm coef vcov
#' @importFrom ggplot2 ggplot aes geom_point geom_errorbar geom_hline
#'   labs theme_minimal
#' @export
test_pretrends <- function(model = NULL,
                           betahat = NULL,
                           sigma = NULL,
                           pre_periods = NULL,
                           conf_level = 0.95,
                           honest_did = FALSE) {

  # --- Extract coefficients and vcov ---
  if (!is.null(model)) {
    if (!inherits(model, "fixest")) {
      stop("`model` must be a fixest object.", call. = FALSE)
    }
    all_coefs <- stats::coef(model)
    all_vcov  <- stats::vcov(model)

    # Auto-detect pre-treatment periods from coefficient names
    if (is.null(pre_periods)) {
      coef_names <- names(all_coefs)
      # Match names that contain a negative number (e.g., "time_to_treatment::-2")
      pre_idx <- grep("::-[0-9]+$|\\.-[0-9]+$|_-[0-9]+$", coef_names)
      if (length(pre_idx) == 0) {
        stop(
          "Could not auto-detect pre-treatment periods. ",
          "Please supply `pre_periods` explicitly.",
          call. = FALSE
        )
      }
      pre_periods <- pre_idx
    }

    betahat <- unname(all_coefs[pre_periods])
    sigma   <- all_vcov[pre_periods, pre_periods, drop = FALSE]
    pre_names <- names(all_coefs)[pre_periods]
  } else {
    if (is.null(betahat) || is.null(sigma)) {
      stop("Either `model` or both `betahat` and `sigma` must be provided.",
           call. = FALSE)
    }
    if (is.null(pre_periods)) {
      pre_periods <- seq_along(betahat)
    }
    pre_names <- paste0("pre_", seq_along(betahat))
  }

  k <- length(betahat)
  if (k == 0) {
    stop("No pre-treatment coefficients found.", call. = FALSE)
  }

  # --- Joint F-test: H0: all pre-treatment coefs = 0 ---
  sigma_inv <- tryCatch(
    solve(sigma),
    error = function(e) {
      stop("Variance-covariance matrix is singular.", call. = FALSE)
    }
  )
  f_stat <- as.numeric(t(betahat) %*% sigma_inv %*% betahat) / k

  # Degrees of freedom
  if (!is.null(model)) {
    df2 <- fixest::degrees_freedom(model, type = "resid")
  } else {
    df2 <- Inf
  }
  p_value <- stats::pf(f_stat, df1 = k, df2 = df2, lower.tail = FALSE)

  joint_test <- list(
    f_stat  = f_stat,
    df1     = k,
    df2     = df2,
    p_value = p_value
  )

  # --- Individual coefficient tests ---
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  se <- sqrt(diag(sigma))
  t_stats <- betahat / se
  individual_p <- 2 * stats::pnorm(-abs(t_stats))

  individual_tests <- data.frame(
    term      = pre_names,
    estimate  = betahat,
    std_error = se,
    t_stat    = t_stats,
    p_value   = individual_p,
    ci_lower  = betahat - z * se,
    ci_upper  = betahat + z * se,
    stringsAsFactors = FALSE
  )
  rownames(individual_tests) <- NULL

  # --- Plot ---
  plot_df <- individual_tests
  plot_df$term <- factor(plot_df$term, levels = plot_df$term)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = term, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2
    ) +
    ggplot2::labs(
      x = "Pre-Treatment Period",
      y = "Coefficient Estimate",
      title = "Pre-Trends Test",
      caption = paste0(
        "Joint F-test: F = ", round(f_stat, 3),
        ", p = ", format.pval(p_value, digits = 3)
      )
    ) +
    ggplot2::theme_minimal()

  # --- Optional HonestDiD ---
  honest_result <- NULL
  if (honest_did) {
    if (!requireNamespace("HonestDiD", quietly = TRUE)) {
      warning(
        "Package 'HonestDiD' is not installed. Skipping sensitivity analysis.\n",
        "Install with: remotes::install_github('asheshrambachan/HonestDiD')",
        call. = FALSE
      )
    } else {
      honest_result <- tryCatch({
        HonestDiD::createSensitivityResults(
          betahat = betahat,
          sigma   = sigma,
          numPrePeriods  = k,
          numPostPeriods = 0
        )
      }, error = function(e) {
        warning("HonestDiD sensitivity analysis failed: ", e$message,
                call. = FALSE)
        NULL
      })
    }
  }

  list(
    joint_test       = joint_test,
    individual_tests = individual_tests,
    plot             = p,
    honest_did       = honest_result
  )
}
