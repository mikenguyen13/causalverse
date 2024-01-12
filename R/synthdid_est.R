#' Synthetic DID Estimation Using synthdid Package
#'
#' This function estimates synthetic difference-in-differences using the `synthdid` package.
#' It offers a choice among `synthdid_estimate`, `did_estimate`, and `sc_estimate` methods 
#' for estimation, defaulting to `synthdid_estimate`. It calculates treatment effects (TEs)
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
#' @param method The estimation method to be used. Methods include:
#'   - 'did': Difference-in-Differences.
#'   - 'sc': Synthetic Control Method. 
#'   - 'sc_ridge': Synthetic Control Method with Ridge Penalty. It adds a ridge regularization to the synthetic control method when estimating the synthetic control weights.
#'   - 'difp': De-meaned Synthetic Control Method, as proposed by Doudchenko and Imbens (2016) and Ferman and Pinto (2021).
#'   - 'difp_ridge': De-meaned Synthetic Control with Ridge Penalty. It adds a ridge regularizationd when estimating the synthetic control weights.
#'   - 'synthdid': Synthetic Difference-in-Differences, a method developed by Arkhangelsky et al. (2021)
#' Defaults to 'synthdid'.
#' @references
#' Ferman, B., & Pinto, C. (2021). Synthetic controls with imperfect pretreatment fit. 
#' Quantitative Economics, 12(4), 1197-1221.
#'
#' Doudchenko, Nikolay, and Guido W. Imbens. 2016. 
#' “Balancing, Regression, Difference-in-Differences and Synthetic Control Methods: A Synthesis.” 
#' NBER Working Paper 22791.
#'
#' Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021). 
#' Synthetic difference-in-differences. 
#' American Economic Review, 111(12), 4088-4118.
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
           seed = 1,
           method = "synthdid") {
    
    
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
    
    # Run estimation based on the specified method
    estimation_res <- switch(
      method,
      "did"      = synthdid::did_estimate(setup$Y, setup$N0, setup$T0),
      "sc"       = synthdid::sc_estimate(setup$Y, setup$N0, setup$T0),
      "sc_ridge" = synthdid::sc_estimate(setup$Y, setup$N0, setup$T0,
                                         eta.omega = ((nrow(setup$Y) - setup$N0) * (ncol(setup$Y) - setup$T0)) ^ (1 / 4)),
      "difp"     = synthdid::synthdid_estimate(setup$Y,
                                               setup$N0,
                                               setup$T0,
                                               weights = list(lambda = rep(1 / setup$T0, setup$T0)),
                                               eta.omega = 1e-6),
      "difp_ridge" = synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0,
                                                 weights = list(lambda = rep(1 / setup$T0, setup$T0))),
      
      # Default case
      synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
    )
    
    
    # Subgroup adjustment (i.e., treated are the only subgroup of interest)
    if (!is.null(subgroup)) {
      # get treated unit IDs
      treat_ids <- rownames(setup$Y[-(1:setup$N0), ])
      # keep only those in subgroup
      keep <- c(rep(TRUE, setup$N0), treat_ids %in% subgroup)
      
      # change the setup matrix to contain only that data.
      estimation_res.setup          <- attr(estimation_res, 'setup')
      estimation_res.setup$Y        <- estimation_res.setup$Y[keep, ]
      estimation_res.setup$X        <- estimation_res.setup$X[keep, ,]
      attr(estimation_res, 'setup') <- estimation_res.setup
    }
    
    # Extract outcomes from synthdid estimates
    setup   <- attr(estimation_res, 'setup')
    weights <- attr(estimation_res, 'weights')
    Y       <- setup$Y - synthdid:::contract3(setup$X, weights$beta)
    N0      <- setup$N0
    T0      <- setup$T0
    
    # Compute treatment effects and standard errors
    est <- synthdid_est_per(Y, N0, T0, weights)
    se  <- synthdid_se_jacknife(estimation_res, seed = seed)
    
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
