#' Calculate Placebo Standard Errors for Synthetic DID
#'
#' Computes placebo standard errors for synthetic difference-in-differences (DID) estimates. This
#' function is based on the methodology described in Arkhangelsky et al. (2021). It is particularly
#' useful when there is only one treated unit and performs a bootstrap procedure to estimate the
#' standard errors.
#'
#' @param estimate An estimate object obtained from synthetic DID estimation.
#' @param replications The number of bootstrap replications to perform. Defaults to 500.
#' @param seed A numeric value for setting the random seed. Default is 1.
#' @return A vector of standard errors corresponding to the input estimates.
#' @references
#' Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021).
#' Synthetic Difference-in-Differences. American Economic Review, 111(12), 4088-4118.
#' American Economic Association 2014 Broadway, Suite 305, Nashville, TN 37203.
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
#'   # get treatment status
#'   dplyr::mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
#'   # correct those control units to have treatment status to be 0
#'   dplyr::mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar))) |>
#'   synthdid::panel.matrices(
#'     unit = "id",
#'     time = "year",
#'     outcome = "y",
#'     treatment = "treatvar"
#'   )
#' estimate <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
#' se_results <- synthdid_se_placebo(estimate, replications = 1000)
#' }
synthdid_se_placebo <- function(estimate, replications = 10000, seed = 1) {
  set.seed(seed)
  # Extract setup, options, and weights from the estimate object
  setup = attr(estimate, 'setup')
  opts = attr(estimate, 'opts')
  weights = attr(estimate, 'weights')
  
  # Determine the number of treated units
  N1 = nrow(setup$Y) - setup$N0
  
  # Validate number of controls versus treated units
  if (setup$N0 <= N1) {
    stop('You need more controls than treated units to run placebo standard errors.')
  }
  
  # Bootstrap function for computing standard errors
  theta <- function(ind) {
    N0 = length(ind) - N1
    weights_boot = weights
    weights_boot$omega = sum_normalize(weights$omega[ind[1:N0]])
    
    # Estimate using the synthdid_est_per function
    est <- do.call(synthdid_est_per,
                      list(Y = setup$Y[ind, ],
                           N0 = N0,
                           T0 = setup$T0,
                           weights = weights_boot))
    est$est
  }
  
  # Compute and return the standard errors
  return(sqrt((replications - 1) / replications) * apply(replicate(replications, theta(sample(1:setup$N0))), 1, sd))
}
