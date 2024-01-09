#' Synthetic DID Estimation Using synthdid Package
#'
#' This function estimates synthetic difference-in-differences using the `synthdid` package.
#' It differs from `synthdid::synthdid_estimate` in that it calculates treatment effects (TEs)
#' for each period instead of a single TE for all treated periods.
#'
#' @param data Data frame to analyze.
#' @param adoption_cohort Cohort in data to use as treated.
#' @param subgroup (Optional) List of IDs to use as treated subgroup.
#' @param lags Number of lags to use pre-treatment.
#' @param leads Number of post-treatment periods (0 for only the treatment period).
#' @param time_var Name of the calendar time column.
#' @param unit_id_var Name of the unit ID column.
#' @param treated_period_var Name of the treatment time period column.
#' @param treat_stat_var Name of the treatment indicator column.
#' @param outcome_var Name of the outcome variable column.
#' @param seed A numeric value for setting the random seed (only for placebo SE). Default is 1.
#' @return A list containing the estimated treatment effects, standard errors, observed and predicted outcomes, synthetic control lambda weights, and counts of treated and control units.
#' @export
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   library(causalverse)
#'   library(synthdid)
#'
#'   data <- get_balanced_panel(
#'     data = fixest::base_stagg,
#'     adoption_cohort = 5,
#'     lags = 2,
#'     leads = 3,
#'     time_var = "year",
#'     unit_id_var = "id",
#'     treated_period_var = "year_treated"
#'   ) |>
#'     dplyr::mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
#'     dplyr::mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar)))
#'
#'   synthdid_est(
#'     data,
#'     adoption_cohort = 5,
#'     lags = 2,
#'     leads = 3,
#'     time_var = "year",
#'     unit_id_var = "id",
#'     treated_period_var = "year_treated",
#'     treat_stat_var = "treatvar",
#'     outcome_var = "y"
#'   )
#' }
synthdid_est <-
  function(data,
           adoption_cohort,
           subgroup = NULL,
           lags,
           leads,
           time_var,
           unit_id_var,
           treated_period_var,
           treat_stat_var,
           outcome_var, 
           seed = 1) {
    
    
    # Convert treatment variable to logical and select necessary columns
    data <- data %>%
      mutate(!!treat_stat_var := as.logical(!!sym(treat_stat_var))) %>%
      select(!!treat_stat_var,!!time_var,!!unit_id_var,!!treated_period_var,!!outcome_var)
    
    # Remove units with missing data in a time period
    if (any(is.na(data))) {
      data <- as.data.table(data) %>%
        .[, nas := min(get(outcome_var), na.rm = TRUE), by = !!unit_id_var] %>%
        na.omit() %>%
        .[, nas := NULL] %>%
        as.data.frame()
    }
    
    # Setup data structure for synthdid package
    setup <-
      synthdid::panel.matrices(
        panel     = data,
        unit      = unit_id_var,
        time      = time_var,
        outcome   = outcome_var,
        treatment = treat_stat_var
      )
    
    # Run synthdid estimation
    sdid <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
    
    # Subgroup adjustment (i.e., treated are the only subgroup of interest)
    if (!is.null(subgroup)) {
      # get treated unit IDs
      treat_ids <- rownames(setup$Y[-(1:setup$N0), ])
      
      keep <- c(rep(TRUE, setup$N0), treat_ids %in% subgroup)
      sdid.setup <- attr(sdid, 'setup')
      sdid.setup$Y <- sdid.setup$Y[keep, ]
      sdid.setup$X <- sdid.setup$X[keep, , drop = FALSE]
      attr(sdid, 'setup') <- sdid.setup
    }
    
    # Extract outcomes from synthdid estimates
    setup   <- attr(sdid, 'setup')
    weights <- attr(sdid, 'weights')
    Y       <- setup$Y - synthdid:::contract3(setup$X, weights$beta)
    N0      <- setup$N0
    T0      <- setup$T0
    
    # Compute treatment effects and standard errors
    est <- synthdid_est_per(Y, N0, T0, weights)
    se  <- synthdid_se_jacknife(sdid, seed = seed)
    
    return(
      list(
        est          = est$est,
        se           = se,
        y_pred       = est$y_pred,
        y_obs        = est$y_obs,
        lambda.synth = est$lambda.synth,
        Ntr          = est$Ntr,
        Nco          = est$Nco
      )
    )
  }
