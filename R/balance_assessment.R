#' Assess balance between treated and control groups
#'
#' This function performs a balance assessment between treated and control groups using 
#' Seemingly Unrelated Regression (SUR) and Hotelling's T-squared test.
#'
#' @param data A dataframe containing the data to be assessed.
#' @param treatment_col The name of the column that contains the treatment indicator (0 for control, 1 for treated).
#' @param ... Names of the dependent variables.
#'
#' @return A list with two elements: 'SUR' (results of the SUR) and 'Hotelling' (results of the Hotelling's T-squared test).
#' @importFrom dplyr select rowwise mutate ungroup
#' @importFrom systemfit systemfit
#' @importFrom Hotelling hotelling.test
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' set.seed(123)
#' data = mtcars %>% 
#'   dplyr::select(mpg, cyl, disp, hp, wt) %>% 
#'   dplyr::rowwise() %>% 
#'   dplyr::mutate(treatment = sample(c(0,1), 1, replace = TRUE)) %>% 
#'   dplyr::ungroup()
#'
#' results <- balance_assessment(data, "treatment", "mpg", "cyl")
#' print(results$SUR)
#' print(results$Hotelling)
#' }
#' @export
balance_assessment <- function(data, treatment_col, ...){
  dep_vars <- as.character(substitute(list(...)))[-1L] # Convert ... to character vector
  
  # Create SUR equations
  equations <- lapply(dep_vars, function(var) {
    as.formula(paste(var, "~", treatment_col))
  })
  
  system_names <- paste0(dep_vars, "eq")
  system <- setNames(equations, system_names)
  
  fit <- systemfit(system, data = data, method = "SUR")
  sur_summary <- summary(fit)
  
  # Hotelling Test
  treated_data <- data[data[[treatment_col]] == 1, dep_vars]
  control_data <- data[data[[treatment_col]] == 0, dep_vars]
  
  hotelling_test_res <- hotelling.test(treated_data, control_data)
  
  # Return results
  list(
    SUR = sur_summary,
    Hotelling = hotelling_test_res
  )
}
