#' Stacked Data for Staggered DiD Analysis
#'
#' `stack_data` processes datasets used in staggered Difference-in-Differences (DiD) designs.
#' Staggered DiD designs arise when different units (e.g., firms, regions, countries) 
#' get treated at different time periods. This function creates cohorts based on the provided 
#' treatment period variable and stacks them together to create a comprehensive longitudinal format 
#' suitable for staggered DiD analyses.
#'
#' The function emphasizes the importance of having a control group, which should be represented by 
#' the value 10000 in the `treated_period_var` column of the provided dataset. The output data will 
#' be augmented with relative period dummy variables for ease of subsequent analysis.
#'
#' @param treated_period_var A character string indicating the column name of the treatment period variable.
#' @param time_var A character string indicating the column name for time.
#' @param pre_window An integer indicating the number of periods before the treatment to consider.
#' @param post_window An integer indicating the number of periods after the treatment to consider.
#' @param data A data frame containing the dataset to be processed.
#' @param control_type A character string indicating which control type to use. One of "both", "never-treated", or "not-yet-treated".
#'
#' @return A data frame with the stacked data, augmented with relative period dummy variables, 
#' suitable for staggered DiD analysis.
#' @examples
#' \dontrun{
#'   library(did)
#'   library(tidyverse)
#'   library(fixest)
#'   data(base_stagg)
#'   stacked_data <- stack_data("year_treated", "year", 3, 3, base_stagg, control_type = "both")
#'   feols_result <- feols(as.formula(paste0(
#'     "y ~ ",
#'     paste(paste0("`rel_period_", c(-3:-2, 0:3), "`"), collapse = " + "),
#'     " | id ^ df + year ^ df"
#'   )), data = stacked_data)
#'   print(feols_result)
#' }
#' @export

stack_data <- function(treated_period_var, time_var, pre_window, post_window, data, control_type = c("both", "never-treated", "not-yet-treated")) {
  
  control_type <- base::match.arg(control_type) # Ensures only valid values are passed
  
  # Ensure that treated_period_var contains 10000
  if (control_type != "not-yet-treated" && !any(data[[treated_period_var]] == 10000)) {
    warning("The dataset does not contain a value of 10000 in the treated_period_var. This means there is no 'never treated' group.")
  }
  
  # Get treatment cohorts
  cohorts <- data %>%
    dplyr::select(!!rlang::sym(treated_period_var)) %>%
    # Exclude never-treated group
    dplyr::filter(!!rlang::sym(treated_period_var) != 10000) %>%
    dplyr::distinct() %>%
    dplyr::pull()
  
  # Inner function to process data
  getdata <- function(focal_treated_period, treated_period_var, time_var, pre_window, post_window, data, control_type) {
    if (control_type == "never-treated") {
      data %>%
        dplyr::filter((!!rlang::sym(treated_period_var)) == focal_treated_period | (!!rlang::sym(treated_period_var)) == 10000) %>%
        dplyr::filter((!!rlang::sym(time_var)) >= (focal_treated_period - pre_window) & (!!rlang::sym(time_var)) <= (focal_treated_period + post_window)) %>%
        dplyr::mutate(df = focal_treated_period)
    } else if (control_type == "not-yet-treated") {
      data %>%
        dplyr::filter((!!rlang::sym(treated_period_var)) == focal_treated_period | (!!rlang::sym(treated_period_var)) > (focal_treated_period + post_window)) %>%
        dplyr::filter((!!rlang::sym(time_var)) >= (focal_treated_period - pre_window) & (!!rlang::sym(time_var)) <= (focal_treated_period + post_window)) %>%
        dplyr::filter(!!rlang::sym(treated_period_var) != 10000) %>% # do not include never-treated
        dplyr::mutate(df = focal_treated_period)
    } else { # control_type == "both"
      data %>%
        dplyr::filter((!!rlang::sym(treated_period_var)) == focal_treated_period | (!!rlang::sym(treated_period_var)) > (focal_treated_period + post_window)) %>%
        dplyr::filter((!!rlang::sym(time_var)) >= (focal_treated_period - pre_window) & (!!rlang::sym(time_var)) <= (focal_treated_period + post_window)) %>%
        dplyr::mutate(df = focal_treated_period)
    }
  }
  
  # Process and return the data
  stacked_data <- purrr::map_df(
    cohorts,
    ~ getdata(
      focal_treated_period = .,
      treated_period_var   = treated_period_var,
      time_var             = time_var,
      pre_window           = pre_window,
      post_window          = post_window,
      data                 = data,
      control_type         = control_type
    )
  ) %>%
    dplyr::mutate(rel_period = dplyr::if_else(df == !!rlang::sym(treated_period_var), !!rlang::sym(time_var) - df, NA_real_)) %>%
    fastDummies::dummy_cols("rel_period", ignore_na = TRUE) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("rel_period_"), ~ tidyr::replace_na(., 0)))
  
  return(stacked_data)
}
