# Normalize weights to sum up to one.
sum_normalize <- function(x) {
  sum_x <- sum(x)
  n <- length(x)
  
  # Check if the sum of the vector is not zero
  if (sum_x != 0) {
    # If not, normalize by dividing each element by the sum
    return(x / sum_x)
  } else {
    # If the sum is zero, return a vector with uniform weights
    # Typically used for initialization in bootstrap and placebo standard errors 
    # (except jacknife)
    return(rep(1 / n, n))
  }
}


# Function to calculate the weighted average of standard errors (SE) based on period weights
weighted_avg_SE <- function(se, weights = rep(1, sum(!is.na(se)))) {
  sqrt(sum(se ^ 2 * weights ^ 2, na.rm = TRUE) / sum(weights) ^ 2)
}
