#'Calculate Jackknife Standard Errors for Synthetic DID
#'
#' Computes the standard error of estimates using the jackknife method. It is specifically tailored for use with
#' synthetic difference-in-differences estimates from the `synthdid` package. This function supports both the usual
#' jackknife estimate of variance and the fixed-weights jackknife estimate as described by Arkhangelsky et al.
#'
#' @param estimate A synthdid estimate object.
#' @param weights Optional; custom weights for the fixed-weights jackknife. If NULL, the usual jackknife estimate is calculated.
#' @param seed A numeric value for setting the random seed (only for placebo SE). Default is 1.
#' 
#' @return Returns the standard error of the provided estimate.
#' @importFrom stats var
#' @references 
#' Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021). Synthetic difference-in-differences.
#' American Economic Review, 111(12), 4088-4118.
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
#' se_results <- synthdid_se_jacknife(estimate, seed = 123)
#' }
synthdid_se_jacknife <-
  function(estimate,
           weights = attr(estimate, 'weights'),
           seed = 1) {
    
    setup = attr(estimate, 'setup')
    opts = attr(estimate, 'opts')

    if (!is.null(weights)) {
      opts$update.omega = opts$update.lambda = FALSE
    }

    if (setup$N0 == nrow(setup$Y) - 1) {
      print("Since you have only 1 treated unit, we have to use the placebo SE method.")
      # Using synthdid_se_placebo instead of jackknife
      return(synthdid_se_placebo(estimate, seed = seed))
    }

    if (!is.null(weights) &&
        sum(weights$omega != 0) == 1) {
      return(NA)
    }

    theta = function(ind) {
      weights_jack = weights
      if (!is.null(weights)) {
        weights_jack$omega = sum_normalize(weights$omega[ind[ind <= setup$N0]])
      }
      est = do.call(synthdid_est_per,
                    list(
                      Y       = setup$Y[ind, ],
                      N0      = sum(ind <= setup$N0),
                      T0      = setup$T0,
                      weights = weights_jack
                    ))
      est$est
    }

    # Since we have the treatment effects for each period,
    # we have to compute the variance for each time period.
    x = 1:nrow(setup$Y)
    n = length(x)
    u = NULL

    for (i in 1:n) {
      u = rbind(u, theta(x[-i]))
    }

    return(sqrt(apply(u, 2, var) * ((n - 1) / n) * (n - 1)))
  }
