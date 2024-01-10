#' Estimate the SynthDiD ATEs and Standard Errors
#'
#' This function uses an adapted SynthDiD method (Arkhangelsky et al., 2021) to
#' estimate the average treatment effect for staggered adoption scenarios. It combines
#' cohort-level ATT estimates, similar to the approach in Ben-Michael et al. (2022),
#' for synthetic controls with staggered adoption. The function is designed to handle
#' various cohorts, lags, leads, placebo tests, and pooled analyses.
#'
#' @references
#' Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., & Wager, S. (2021).
#' Synthetic difference-in-differences. American Economic Review, 111(12), 4088-4118.
#' American Economic Association 2014 Broadway, Suite 305, Nashville, TN 37203.
#'
#' Ben-Michael, E., Feller, A., & Rothstein, J. (2022). Synthetic controls with staggered
#' adoption. Journal of the Royal Statistical Society Series B: Statistical Methodology,
#' 84(2), 351-381. Oxford University Press.
#'
#' @param data A data frame in long format to be analyzed.
#' @param adoption_cohorts Vector of cohorts to use for adoption times.
#' @param lags Integer, number of lags of adoption time to analyze.
#' @param leads Integer, number of leads of adoption time to analyze.
#' @param time_var String, column name of time variables.
#' @param unit_id_var String, ID column of units.
#' @param treated_period_var String, column with adoption time of each unit.
#' @param treat_stat_var String, column name indicating treatment status.
#' @param outcome_var String, column of outcome to analyze.
#' @param placebo Logical, whether to run placebo analysis.
#' @param pooled Logical, whether to run pooled analysis of all treated units.
#' @param subgroup Vector, IDs for subgroup analysis.
#' @param conf_level Numeric, confidence level for the interval estimation (Default: 95%).
#' @param seed A numeric value for setting the random seed (for placebo SE and placebo analysis). Default is 1.
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item{time}{: Vector of time periods used in estimation from -lags to leads (relative to the adoption period)}
#'   \item{TE_mean}{: Vector of ATT in each time period}
#'   \item{SE_mean}{: Vector of Standard error of ATT each time period}
#'   \item{TE_mean_lower}{: Vector of Lower C.I. for ATT per period}
#'   \item{TE_mean_upper}{: Vector of Upper C.I. for ATT per period}
#'   \item{TE_mean_w, SE_mean_w, TE_mean_w_lower, TE_mean_w_upper}{: Weighted versions of the above metrics by the number of treated units in each time period }
#'   \item{Ntr}{: Number of treated units}
#'   \item{Nco}{: Number of control units}
#'   \item{TE}{: Treatment effect for each cohort in each time period}
#'   \item{SE}{: Standard error of TE of each cohort in each time period}
#'   \item{y_obs}{: Observed outcomes of treated units}
#'   \item{y_pred}{: Predicted outcomes of treated units}
#'   \item{col_names}{: Column names for TE and SE matrices (times and ATTs)}
#' }
#' @export
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   data <- fixest::base_stagg |>
#'     mutate(treatvar = if_else(time_to_treatment >= 0, 1, 0)) |>
#'     mutate(treatvar = as.integer(if_else(year_treated > (5 + 2), 0, treatvar)))
#'
#'   synthdid_est_ate(
#'     data = data,
#'     adoption_cohorts = 5:7,
#'     lags = 2,
#'     leads = 2,
#'     time_var = "year",
#'     unit_id_var = "id",
#'     treated_period_var = "year_treated",
#'     treat_stat_var = "treatvar",
#'     pooled = F,
#'     outcome_var = "y"
#'   )
#' }
synthdid_est_ate <-
  function(data,
           adoption_cohorts,
           lags,
           leads,
           time_var,
           unit_id_var ,
           treated_period_var ,
           treat_stat_var ,
           outcome_var,
           placebo = F,
           pooled = F,
           subgroup = NULL,
           conf_level = 0.95, 
           seed = 1
  ) {
    set.seed(seed)
    # Validate input data
    # Ensure input data is a data frame
    if (!is.data.frame(data)) {
      stop("Input 'data' must be a data frame.")
    }
    
    # Initialize return variables
    TE <- SE <- y_pred <- y_obs <- Ntr <- Nco <- out_adoption_cohorts <- NULL
    
    # Process each adoption cohort
    for (adoption_cohort in adoption_cohorts) {
      cat("adoption_cohort:",adoption_cohort,"\n" )
      
      # Prepare balanced panel data for current adoption cohort
      # Specifically, create balanced panel with lags and leads for all treated
      # units in adoption_cohort as treated and all units that are either 
      # later-treated (outside of the current window of 
      # observation = treated_period + lags) or never-treated as control
      balanced_df = causalverse::get_balanced_panel(
        data = data,
        adoption_cohort = adoption_cohort,
        lags = lags,
        leads = leads,
        time_var = time_var,
        unit_id_var = unit_id_var,
        treated_period_var = treated_period_var
      )
      # Get number of treated units
      n_treat = length(unique(balanced_df[balanced_df[, treated_period_var] == adoption_cohort, unit_id_var]))
      
      # For placebo analysis, choose a random set of control units to act as 
      # treated ones and then reorganize the data accordingly.
      if (placebo) { 
        print("Running Placebo Analysis")
        
        placebo_treat_ids <- balanced_df %>%
          dplyr::filter(.data[[treated_period_var]] > adoption_cohort) %>%
          dplyr::pull(.data[[unit_id_var]]) %>%
          unique() |> 
          
          # random sample placebo units without replacement
          sample(n_treat, replace = F)
        
        balanced_df <- balanced_df %>%
          
          filter(.data[[treated_period_var]] > adoption_cohort) %>%
          
          mutate(
            !!treated_period_var := dplyr::if_else(.data[[unit_id_var]] %in% placebo_treat_ids, adoption_cohort, .data[[treated_period_var]]),
            !!treat_stat_var := dplyr::if_else(.data[[time_var]] >= .data[[treated_period_var]], 1, .data[[treat_stat_var]])
          )
      }
      
      
      # For pooled analysis, substitute the data of treated units with 
      # the average outcome from all treated units.
      if (pooled) {
        print("Running Pooled Analysis")
        
        treat_data <- balanced_df %>%
          dplyr::filter(.data[[treated_period_var]] == adoption_cohort) %>%
          dplyr::select(time_var,
                        treat_stat_var,
                        treated_period_var,
                        outcome_var,
                        unit_id_var)
        
        balanced_df <- balanced_df %>%
          dplyr::filter(.data[[treated_period_var]] > adoption_cohort) %>%
          dplyr::select(time_var,
                        treat_stat_var,
                        treated_period_var,
                        outcome_var,
                        unit_id_var)
        
        
        
        for (time in seq(adoption_cohort-lags,adoption_cohort-1)) {
          new_data = c(time, 0,adoption_cohort,mean(treat_data[treat_data[,treated_period_var]==adoption_cohort,outcome_var]),0)
          balanced_df=rbind(balanced_df,new_data)
        }

        for (time in seq(adoption_cohort+0,adoption_cohort+leads)) {
          new_data = c(time, 1,adoption_cohort,mean(balanced_df[balanced_df[,treated_period_var]==adoption_cohort,outcome_var]),0)
          balanced_df=rbind(balanced_df,new_data)
        }
      }
      # Ensure non-treated units are marked correctly
      # (i.e., those outside of the adoption cohort are control)
      balanced_df <- balanced_df %>%
        mutate(!!treat_stat_var := ifelse(.data[[treated_period_var]] > (adoption_cohort + lags), 0, .data[[treat_stat_var]]))
      
      
      # adjust number of treated and control
      n_control = length(unique(balanced_df[balanced_df[,treated_period_var]>adoption_cohort,unit_id_var]))
      n_treat   = length(unique(balanced_df[balanced_df[,treated_period_var]==adoption_cohort, unit_id_var]))
      
      if (!is.null(subgroup)) {
        n_treat = sum(unique(balanced_df[balanced_df[,treated_period_var]==adoption_cohort, unit_id_var]) %in% subgroup)
      }
      
      cat("Treated units:", n_treat,"Control units:", n_control ,"\n")
      
      # Now, run the synthdid with staggered adoption
      if (n_treat > 0) {
        Ntr = c(Ntr, n_treat)
        Nco = c(Nco, n_control)
        
        # Initialized adoption cohorts that were analyzed
        out_adoption_cohorts = c(out_adoption_cohorts, adoption_cohort)
        
        
        sdid = causalverse::synthdid_est(
          data               = balanced_df,
          adoption_cohort    = adoption_cohort,
          subgroup           = subgroup,
          lags               = lags,
          leads              = leads,
          time_var           = time_var,
          unit_id_var        = unit_id_var,
          treated_period_var = treated_period_var,
          treat_stat_var     = treat_stat_var,
          outcome_var        = outcome_var,
          seed               = seed
        )
        
        # Save output
        TE     = rbind(TE, sdid$est)
        SE     = rbind(SE, sdid$se)
        y_pred = rbind(y_pred, sdid$y_pred)
        y_obs  = rbind(y_obs, sdid$y_obs)
      }
    }
    
    # Aggregate adoption-cohort-level ATTs 
    time         <-  seq(-lags, leads)
    col_names       <-  c(time, paste("cumul.", 0:leads, sep = ""))
    TE           <- data.frame(TE)
    colnames(TE) <- col_names
    rownames(TE) <- out_adoption_cohorts
    
    TE_mean_w    <- colSums(TE * Ntr / sum(Ntr))
    TE_mean      <- colMeans(TE)
    
    SE           <- data.frame(SE)
    colnames(SE) <- col_names
    rownames(SE) <- out_adoption_cohorts
    SE_mean_w    <- apply(SE, 2, weighted_avg_SE, Ntr)
    SE_mean      <- apply(SE, 2, weighted_avg_SE)
    
    # Calculate the z-value for the confidence interval
    z_val           <- qnorm((1 + conf_level) / 2)
    
    TE_mean_lower   <-  TE_mean - z_val * SE_mean
    TE_mean_upper   <-  TE_mean + z_val * SE_mean
    
    TE_mean_w_lower <-  TE_mean_w - z_val * SE_mean_w
    TE_mean_w_upper <-  TE_mean_w + z_val * SE_mean_w
    
    colnames(y_obs) <- colnames(y_pred) <- col_names
    
    return(
      list(
        TE_mean         = TE_mean,
        SE_mean         = SE_mean,
        TE_mean_lower   = TE_mean_lower,
        TE_mean_upper   = TE_mean_upper,
        TE_mean_w       = TE_mean_w,
        SE_mean_w       = SE_mean_w,
        TE_mean_w_lower = TE_mean_w_lower,
        TE_mean_w_upper = TE_mean_w_upper,
        Ntr             = Ntr,
        Nco             = Nco,
        TE              = TE,
        SE              = SE,
        y_obs           = y_obs,
        y_pred          = y_pred,
        time            = time,
        col_names       = col_names
      )
    )
  }
