#' Process Panel Estimate
#'
#' This function processes the output from `panel_estimate()` for panel estimates and returns a formatted data frame.
#' It takes a list of results, each corresponding to a different method, and combines them into a single data frame.
#' The data frame includes the method name, estimate, and standard error for each method.
#'
#' @param results_selected A list of results from `panel_estimate()`. Each element in the list should be an object
#' containing the results for a particular estimation method. Each object must have an `estimate` and a `std.error` attribute.
#'
#' @return A data frame with columns `Method`, `Estimate`, and `SE`, representing the method name, the estimate value,
#' and the standard error, respectively. The data frame is formatted using `causalverse::nice_tab()`.
#' 
#' @examples
#' \dontrun{
#' library(synthdid)
#' setup = synthdid::panel.matrices(synthdid::california_prop99)
#' results_selected = panel_estimate(setup, selected_estimators = c("did", "sc"))
#' results_table = process_panel_estimate(results_selected)
#' print(results_table)
#' }
#' 
#' @export
process_panel_estimate <- function(results_selected) {
  # Create the data frame from the results
  results_df <- do.call(rbind, lapply(names(results_selected), function(name) {
    data.frame(
      Method = toupper(name),
      Estimate = as.numeric(results_selected[[name]]$estimate),
      SE = as.numeric(results_selected[[name]]$std.error)
    )
  })) |> 
    causalverse::nice_tab()
  
  return(results_df)
}
