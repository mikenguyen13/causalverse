#' Compare Causal Estimators in a Formatted Table
#'
#' Takes a named list of treatment effect estimates (either model objects or
#' manual estimate/SE pairs) and produces a formatted comparison data frame
#' with point estimates, standard errors, confidence intervals, and
#' significance stars.
#'
#' @param estimators A named list where each element is either:
#'   \itemize{
#'     \item A model object (from \code{lm}, \code{fixest::feols}, etc.)
#'       from which the coefficient specified by \code{coef_name} will be
#'       extracted.
#'     \item A list with elements \code{$estimate} (numeric) and \code{$se}
#'       (numeric standard error).
#'   }
#' @param coef_name Character string. The coefficient name to extract from
#'   model objects. Required when any element of \code{estimators} is a
#'   model object. Ignored for manual estimate/SE pairs.
#' @param conf_level Numeric. Confidence level for intervals. Default is
#'   \code{0.95}.
#' @param digits Integer. Number of digits for rounding. Default is \code{3}.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{model}{Character. Name of the estimator.}
#'     \item{estimate}{Numeric. Point estimate.}
#'     \item{se}{Numeric. Standard error.}
#'     \item{ci_lower}{Numeric. Lower bound of the confidence interval.}
#'     \item{ci_upper}{Numeric. Upper bound of the confidence interval.}
#'     \item{stars}{Character. Significance stars
#'       (\code{"***"} p < 0.001, \code{"**"} p < 0.01,
#'        \code{"*"} p < 0.05, \code{"."} p < 0.10).}
#'   }
#'
#' @examples
#' \dontrun{
#' # From model objects
#' m1 <- lm(mpg ~ am, data = mtcars)
#' m2 <- lm(mpg ~ am + wt, data = mtcars)
#'
#' compare_estimators(
#'   estimators = list("Bivariate" = m1, "Adjusted" = m2),
#'   coef_name = "am"
#' )
#'
#' # From manual estimates
#' compare_estimators(
#'   estimators = list(
#'     "DID"    = list(estimate = 2.5, se = 0.8),
#'     "SC"     = list(estimate = 2.1, se = 1.0),
#'     "SDID"   = list(estimate = 2.3, se = 0.9)
#'   )
#' )
#' }
#'
#' @importFrom stats qnorm pnorm coef
#' @export
compare_estimators <- function(estimators,
                               coef_name = NULL,
                               conf_level = 0.95,
                               digits = 3) {

  if (!is.list(estimators) || is.null(names(estimators))) {
    stop("`estimators` must be a named list.", call. = FALSE)
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  rows <- lapply(names(estimators), function(name) {
    obj <- estimators[[name]]

    # Manual estimate/se pair
    if (is.list(obj) && all(c("estimate", "se") %in% names(obj))) {
      est <- obj$estimate
      se  <- obj$se
    } else {
      # Model object
      if (is.null(coef_name)) {
        stop(
          "`coef_name` is required when `estimators` contains model objects. ",
          "Problem with estimator '", name, "'.",
          call. = FALSE
        )
      }

      extracted <- tryCatch({
        if (inherits(obj, "fixest")) {
          ct <- fixest::coeftable(obj)
          list(estimate = ct[coef_name, 1], se = ct[coef_name, 2])
        } else {
          ct <- summary(obj)$coefficients
          list(estimate = ct[coef_name, 1], se = ct[coef_name, 2])
        }
      }, error = function(e) {
        warning(
          "Could not extract '", coef_name, "' from estimator '", name,
          "': ", e$message,
          call. = FALSE
        )
        list(estimate = NA_real_, se = NA_real_)
      })
      est <- extracted$estimate
      se  <- extracted$se
    }

    data.frame(
      model    = name,
      estimate = est,
      se       = se,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Confidence intervals
  out$ci_lower <- out$estimate - z * out$se
  out$ci_upper <- out$estimate + z * out$se

  # Significance stars based on two-sided p-value
  p_values <- 2 * stats::pnorm(-abs(out$estimate / out$se))
  out$stars <- vapply(p_values, function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    if (p < 0.10)  return(".")
    ""
  }, character(1))

  # Round numeric columns
  num_cols <- c("estimate", "se", "ci_lower", "ci_upper")
  out[num_cols] <- lapply(out[num_cols], round, digits = digits)

  out
}
