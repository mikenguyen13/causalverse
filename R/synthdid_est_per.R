#' Estimate Treatment Effects for Each Period
#'
#' Given the output from the `synthdid::synthdid_estimate` method, this function computes the 
#' treatment effects (TEs) for each post-treatment period, along with the cumulative 
#' average treatment effect (ATE). It also provides observed and predicted outcomes 
#' for treated units, synthetic control weights, and counts of treated and control units.
#'
#' @param Y Data matrix with units as rows and time periods as columns.
#' @param N0 Number of control units.
#' @param T0 Number of pre-treatment periods.
#' @param weights Output from `synthdid`, containing lambda and omega weights.
#' @return A list containing:
#' \itemize{
#'   \item{est}{: TEs for each post-treatment period and cumulative ATEs.}
#'   \item{y_obs}{: Observed outcomes for treated units.}
#'   \item{y_pred}{: Predicted outcomes for treated units.}
#'   \item{lambda.synth}{: Synthetic control lambda weights.}
#'   \item{Ntr}{: Number of treated units.}
#'   \item{Nco}{: Number of control units.}
#' }
#' @export
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(synthdid)
#' library(fixest)
#'
#' setup <- base_did |>
#'   mutate(
#'     id = as.factor(id),
#'     period = as.integer(period),
#'     y = as.double(y),
#'     post = as.integer(post)
#'   ) |>
#'   # Correct treatment 
#'   dplyr::mutate(treatment = as.integer(if_else(treat == 0, 0, post))) |>
#'   synthdid::panel.matrices(unit = "id", time = "period", outcome = "y", treatment = "treatment") 
#'
#' sdid <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
#' synthdid_est_per(setup$Y, setup$N0, setup$T0, weights = attr(sdid, 'weights'))
#' }
synthdid_est_per <- function(Y, N0, T0, weights) {
  # Calculate the number of treated units and post-treatment periods
  N1 = nrow(Y) - N0
  T1 = ncol(Y) - T0
  
  # Extract and adjust lambda and omega weights
  lambda.synth  = c(weights$lambda, rep(0, T1))
  lambda.target = c(rep(0, T0), rep(1 / T1, T1))
  lambda        = lambda.synth + lambda.target
  omega.synth   = c(weights$omega, rep(0, N1))
  omega.target  = c(rep(0, N0), rep(1 / N1, N1))
  
  # Compute intercept offset for treated and control units
  intercept.offset =  c((omega.target - omega.synth) %*% Y %*% lambda.synth)
  
  # Calculate observed and synthetic trajectories
  obs.trajectory = as.numeric(omega.target %*% Y)
  syn.trajectory = as.numeric(omega.synth %*% Y) + intercept.offset
  
  # Calculate observed and predicted outcomes per unit
  y_obs = matrix(t(t(Y[-(1:N0), ]) * lambda), nrow = N1)
  y_pred = t(apply(Y[-(1:N0), ] %*% lambda.synth, 1, function(x)
    x + ((omega.synth %*% Y) - c(omega.synth %*% Y %*% lambda.synth)
    )) * lambda)
  
  # Handle multiple periods and units for cumulative outcomes
  if (T1 > 1) {
    if (N1 > 1) {
      y_obs = cbind(y_obs, t(apply(y_obs[, -(1:T0)], 1, cumsum)))
      y_pred = cbind(y_pred, t(apply(y_pred[, -(1:T0)], 1, cumsum)))
      
    }
    if (N1 == 1) {
      y_obs = matrix(c(y_obs, cumsum(y_obs[, -(1:T0)])), nrow = N1)
      y_pred = matrix(c(y_pred, cumsum(y_pred[, -(1:T0)])), nrow = N1)
      
    }
  }
  
  # Compute treatment effects
  effects = (obs.trajectory - syn.trajectory) * (lambda.synth + lambda.target * T1)
  # Include ATE for post-treat periods
  effects = c(effects, cumsum(effects[-(1:T0)]) / seq_along(effects[-(1:T0)]))
  
  # Return results
  return(
    list(
      est          = effects,
      y_obs        = y_obs,
      y_pred       = y_pred,
      lambda.synth = lambda.synth,
      Ntr          = N1,
      Nco          = N0
    )
  )
}
