#' Extract a Balanced Panel
#'
#' This function extracts a balanced panel from the data for a specific adoption cohort.
#' It drops units with missing observations at any point within its entire specified window (leads + adoption time, adoption time + lags).
#' Units treated in the same period as the adoption cohort are marked as "treated," and
#' units with their first treatment after the leads time of the specified adoption cohort
#' are marked as "control."
#'
#' @param data The dataset to be used.
#' @param adoption_cohort Numeric, the specific adoption cohort.
#' @param lags Numeric, the number of lags.
#' @param leads Numeric, the number of leads.
#' @param time_var String, the name of the time variable.
#' @param unit_id_var String, the name of the unit ID variable.
#' @param treated_period_var String, the name of the treated period variable.
#' @param filter_units Logical, whether to filter only units with data on all time periods within the specified time window. Defaults to TRUE.
#' @return A data frame with the balanced panel.
#' @export
#' @examples
#' \dontrun{
#' get_balanced_panel(data = fixest::base_stagg,
#'                    adoption_cohort = 5,
#'                    lags = 2,
#'                    leads = 3,
#'                    time_var = "year",
#'                    unit_id_var = "id",
#'                    treated_period_var = "year_treated")
#' }

get_balanced_panel <- function(data = data,
                               adoption_cohort,
                               lags,
                               leads,
                               time_var,
                               unit_id_var,
                               treated_period_var,
                               filter_units = TRUE) {
  new_df <- data %>%
    as.data.frame() %>%
    dplyr::filter(
      # Filter rows where the value in 'time_var' is greater than or equal to (adoption_cohort - lags)
      # and less than or equal to (adoption_cohort + leads)
      .data[[time_var]] >= (adoption_cohort - lags) &
        .data[[time_var]] <= (adoption_cohort + leads) &
        
        # Filter rows where 'treated_period_var' is equal to 'adoption_cohort' 
        # or 'treated_period_var' is greater than (adoption_cohort + leads)
        (.data[[treated_period_var]] == adoption_cohort | .data[[treated_period_var]] > (adoption_cohort + leads))
    )
  
  if (filter_units) {
    new_df <- new_df %>%
      # This is a strong filter
      # Filter only units shared by all time periods
      # (i.e., only those units with data on full observation windows will be retained)
      plm::make.pbalanced(balance.type = "shared.individuals",
                          index = c(unit_id_var, time_var))
  }
  
  return(new_df)
}
