#' Calculate Placebo Standard Errors for Synthetic DID
#'
#' Computes placebo standard errors for synthetic difference-in-differences (DID) estimates.
#' This function is based on the methodology described in Arkhangelsky et al. (2021). It is
#' particularly useful when there is only one treated unit and performs a bootstrap procedure
#' to estimate the standard errors.
#'
#' @param estimate An estimate object obtained from \code{synthdid::synthdid_estimate()} or
#'   \code{causalverse::panel_estimate()}.
#' @param replications The number of bootstrap replications to perform. Defaults to 10000.
#' @param seed A numeric value for setting the random seed. Default is 1.
#'
#' @return
#' When there is a single treated unit (\code{N1 = 1}), a **single numeric scalar** giving
#' the placebo standard error. When there are multiple treated units (\code{N1 > 1}), a
#' **named numeric vector** of length \code{N1}, one standard error per treated unit.
#'
#' \emph{Note}: If you have a single treated unit and pass a single
#' \code{synthdid_estimate} object, the return value is a scalar (not a vector).
#' This resolves the apparent discrepancy between the help page description
#' ("vector of SEs") and a one-treated-unit setup.
#'
#' @details
#' The function samples from the donor (control) units to construct placebo treatments
#' and estimates the standard deviation of the resulting null distribution. The return
#' value is automatically simplified to a scalar for single-treated-unit designs.
#'
#' @references
#' Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021).
#' Synthetic Difference-in-Differences. \emph{American Economic Review}, 111(12), 4088-4118.
#'
#' @export
#' @examples
#' \dontrun{
#' setup <- get_balanced_panel(
#'   data = fixest::base_stagg,
#'   adoption_cohort = 5,
#'   lags = 2,
#'   leads = 3,
#'   time_var = "year",
#'   unit_id_var = "id",
#'   treated_period_var = "year_treated"
#' ) |>
#'   dplyr::mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
#'   dplyr::mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar))) |>
#'   synthdid::panel.matrices(
#'     unit = "id",
#'     time = "year",
#'     outcome = "y",
#'     treatment = "treatvar"
#'   )
#' estimate <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
#'
#' # Returns a SCALAR when N1 = 1
#' se <- synthdid_se_placebo(estimate, replications = 200)
#' cat("SE =", se, "\n")
#' }
synthdid_se_placebo <- function(estimate, replications = 10000, seed = 1) {
  set.seed(seed)
  # Extract setup, options, and weights from the estimate object
  setup   <- attr(estimate, 'setup')
  opts    <- attr(estimate, 'opts')
  weights <- attr(estimate, 'weights')

  # Determine the number of treated units
  N1 <- nrow(setup$Y) - setup$N0

  # Validate number of controls versus treated units
  if (setup$N0 <= N1) {
    stop('You need more controls than treated units to run placebo standard errors.')
  }

  # Bootstrap function for computing standard errors
  theta <- function(ind) {
    N0_boot <- length(ind) - N1
    weights_boot <- weights
    weights_boot$omega <- sum_normalize(weights$omega[ind[seq_len(N0_boot)]])

    # Estimate using the synthdid_est_per function
    est <- do.call(synthdid_est_per,
                   list(Y       = setup$Y[ind, ],
                        N0      = N0_boot,
                        T0      = setup$T0,
                        weights = weights_boot))
    est$est
  }

  # Compute replicated estimates
  reps <- replicate(replications, theta(sample(seq_len(setup$N0))))

  # Compute standard errors
  raw_se <- sqrt((replications - 1) / replications) *
    if (is.matrix(reps)) apply(reps, 1, sd) else sd(reps)

  # Simplify to scalar for single treated unit (N1 = 1)
  if (N1 == 1 && length(raw_se) == 1) {
    return(as.numeric(raw_se))
  }
  raw_se
}
