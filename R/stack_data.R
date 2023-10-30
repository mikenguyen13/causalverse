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
#'
#' @return A data frame with the stacked data, augmented with relative period dummy variables, 
#' suitable for staggered DiD analysis.
#' @examples
#' \dontrun{
#'   library(did)
#'   library(fixest)
#'   data(base_stagg)
#'   stacked_data <- stack_data("year_treated", "year", 5, 5, base_stagg)
#'   feols_result <- feols(as.formula(paste0(
#'     "y ~ ",
#'     paste(paste0("`rel_period_", c(-5:-2, 0:5), "`"), collapse = " + "),
#'     " | id ^ df + year ^ df"
#'   )), data = stacked_data)
#'   print(feols_result)
#' }
#' @export
stack_data <- function(treated_period_var, time_var, pre_window, post_window, data) {
  # # Ensure that treated_period_var contains 10000
  # if(!any(data[[treated_period_var]] == 10000)) {
  #   stop("The dataset does not contain a value of 10000 in the treated_period_var (i.e., control group). Please ensure it exists before processing.")
  # }
  if(!any(data[[treated_period_var]] == 10000)) {
    warning("The dataset does not contain a value of 10000 in the treated_period_var. This means there is no 'never treated' group, and the analysis will strictly use the 'not-yet-treated' group as control.")
  }
  
  
  # Get treatment cohorts
  cohorts <- data %>%
    dplyr::select(!!sym(treated_period_var)) %>%
    # Exclude never-treated group
    dplyr::filter(!!sym(treated_period_var) != 10000) %>%
    unique() %>%
    dplyr::pull()
  
  # Inner function to process data
  getdata <- function(focal_treated_period, treated_period_var, time_var, pre_window, post_window, data) {
    data %>%
      filter((!!sym(treated_period_var)) == focal_treated_period | (!!sym(treated_period_var)) > (focal_treated_period + post_window)) %>%
      filter((!!sym(time_var)) >= (focal_treated_period - pre_window) & (!!sym(time_var)) <= (focal_treated_period + post_window)) %>%
      mutate(df = focal_treated_period)
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
      data                 = data
    )
  ) %>% 
    mutate(rel_period = if_else(df == !!sym(treated_period_var), !!sym(time_var) - df, NA_real_)) %>%
    fastDummies::dummy_cols("rel_period", ignore_na = TRUE) %>%
    mutate(across(starts_with("rel_period_"), ~ replace_na(., 0)))
  
  return(stacked_data)
}
