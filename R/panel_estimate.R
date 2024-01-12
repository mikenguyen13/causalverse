#' Panel Estimate Function
#'
#' This function computes estimates and standard errors for panel data using selected estimators.
#' It allows the user to select specific estimators and set parameters for Monte Carlo replications and seed.
#'
#' @param setup A list containing matrices Y, N0, and T0 for panel data analysis.
#' @param selected_estimators A character vector specifying which estimators to use. For example, c("synthdid", "did", "sc", "difp", "mc", "sc_ridge", "difp_ridge") or names(panel_estimators).
#'   Defaults to all available estimators except 'mc'.
#' @param mc_replications The number of Monte Carlo replications for computing standard errors. 
#'   Applicable if the 'mc' estimator is used. Defaults to 200.
#' @param seed An integer value to set the random seed for reproducibility. Defaults to 1.
#'
#' @return A list where each element corresponds to an estimator and contains its estimate and standard error.
#'
#' @examples
#' \dontrun{
#' data('california_prop99')
#' setup = panel.matrices(california_prop99)
#' results_all = panel_estimate(setup)
#' results_selected = panel_estimate(setup, selected_estimators = c("did", "sc"))
#' summary(results_selected$did$estimate)
#' }
#' 
#' @export
panel_estimate <-
  function(setup,
           selected_estimators = setdiff(names(panel_estimators), "mc"),
           mc_replications = 200,
           seed = 1) {
    set.seed(seed)
    # Subset the list of estimators based on user selection
    estimators_to_use = panel_estimators[selected_estimators]
    
    # Compute estimates
    estimates = lapply(estimators_to_use, function(estimator) {
      estimator(setup$Y, setup$N0, setup$T0)
    })
    
    # Compute standard errors
    standard.errors = mapply(function(estimate, name) {
      if (name == 'mc') {
        mc_placebo_se(setup$Y, setup$N0, setup$T0, replications = mc_replications)
      } else {
        sqrt(vcov(estimate, method = 'placebo'))
      }
    }, estimates, names(estimators_to_use))
    
    # Combine estimates and standard errors
    results = Map(function(est, se)
      list(estimate = est, std.error = se),
      estimates,
      standard.errors)
    
    # Name the results with estimator names
    names(results) = names(estimators_to_use)
    
    return(results)
  }
