#' Nice Tabulation Function
#'
#' Create a custom function that takes a data frame and a number of decimal places as input,
#' rounds all numeric columns in the data frame to the specified number of decimal places,
#' and returns the modified data frame.
#'
#' @param data A data frame.
#' @param digit_decimal A number of decimal places.
#' @return A data frame with all numeric columns rounded to the specified number of decimal places.
#' @export
nice_tab <- function(data, digit_decimal = 2) {
  
  # Check if each column is numeric and round if it is
  data <- as.data.frame(lapply(data, function(x) {
    if (is.numeric(x)) return(round(x, digits = digit_decimal))
    return(x)
  }))
  # Return the modified data frame
  return(data)
}