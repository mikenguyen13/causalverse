#' Panel Estimate Function
#'
#' This function computes estimates and standard errors for panel data using selected estimators.
#' It allows the user to select specific estimators and set parameters for Monte Carlo replications
#' and seed. Optionally supports covariate adjustment for the \code{"sdid"} (Synthetic DID) estimator.
#'
#' @param setup A list containing matrices \code{Y}, \code{N0}, and \code{T0} for panel data analysis,
#'   as produced by \code{synthdid::panel.matrices()}.
#' @param selected_estimators A character vector specifying which estimators to use.
#'   Available options: \code{"sc"}, \code{"sdid"}, \code{"did"}, \code{"sc_ridge"},
#'   \code{"difp"}, \code{"difp_ridge"}, \code{"mc"}.
#'   Defaults to all available estimators except \code{"mc"}.
#' @param X A 3-dimensional array of covariates for the Synthetic DID estimator, with dimensions
#'   \code{(N, T, K)} where \code{N} is total units, \code{T} is time periods, and \code{K} is
#'   the number of covariates. Only used when the \code{"sdid"} estimator is selected.
#'   See \code{synthdid::synthdid_estimate()} for details. Default \code{NULL} (no covariates).
#' @param mc_replications The number of Monte Carlo replications for computing standard errors.
#'   Applicable if the \code{"mc"} estimator is used. Defaults to 200.
#' @param seed An integer value to set the random seed for reproducibility. Defaults to 1.
#'
#' @return A list where each element corresponds to an estimator and contains its estimate and
#'   standard error. Each element is a list with:
#'   \describe{
#'     \item{estimate}{The estimator object.}
#'     \item{std.error}{The placebo standard error.}
#'   }
#'
#' @details
#' **Covariate Support**: The \code{X} argument adds covariate adjustment to the Synthetic DID
#' estimator, as described in Arkhangelsky et al. (2021). When covariates are supplied, \code{sdid}
#' will partial out the covariate effects before computing synthetic weights. Other estimators
#' (\code{sc}, \code{did}, etc.) do not support the \code{X} argument and will use
#' \code{setup$Y}, \code{setup$N0}, and \code{setup$T0} directly.
#'
#' @references
#' Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021).
#' Synthetic Difference-in-Differences. \emph{American Economic Review}, 111(12), 4088-4118.
#'
#' @examples
#' \dontrun{
#' library(synthdid)
#' setup <- panel.matrices(california_prop99)
#' results_all <- panel_estimate(setup)
#' results_selected <- panel_estimate(setup, selected_estimators = c("did", "sc", "sdid"))
#' summary(results_selected$sdid$estimate)
#' }
#'
#' @export
panel_estimate <- function(setup,
                            selected_estimators = setdiff(names(panel_estimators), "mc"),
                            X               = NULL,
                            mc_replications = 200,
                            seed            = 1) {
  set.seed(seed)

  # Subset the list of estimators based on user selection
  estimators_to_use <- panel_estimators[selected_estimators]

  # Compute estimates
  estimates <- lapply(names(estimators_to_use), function(name) {
    estimator <- estimators_to_use[[name]]
    if (name == "sdid" && !is.null(X) &&
        requireNamespace("synthdid", quietly = TRUE)) {
      # Pass covariates to synthdid estimator
      tryCatch(
        synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0, X = X),
        error = function(e) {
          warning("Covariate-adjusted synthdid failed: ", e$message,
                  ". Falling back to no-covariate estimate.", call. = FALSE)
          estimator(setup$Y, setup$N0, setup$T0)
        }
      )
    } else {
      estimator(setup$Y, setup$N0, setup$T0)
    }
  })
  names(estimates) <- names(estimators_to_use)

  # Compute standard errors
  standard.errors <- mapply(function(estimate, name) {
    if (name == 'mc') {
      mc_placebo_se(setup$Y, setup$N0, setup$T0, replications = mc_replications)
    } else {
      tryCatch(
        sqrt(stats::vcov(estimate, method = 'placebo')),
        error = function(e) NA_real_
      )
    }
  }, estimates, names(estimators_to_use))

  # Combine estimates and standard errors
  results <- Map(function(est, se) list(estimate = est, std.error = se),
                 estimates, standard.errors)
  names(results) <- names(estimators_to_use)

  return(results)
}
