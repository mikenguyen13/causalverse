#' Summarize Lee Bounds for Always-Takers
#'
#' Computes and summarizes the Lee bounds on the average direct effect for always-takers (ATs) for whom there is a direct effect of treatment (D) on the outcome (Y). This function utilizes \code{\link[MedBounds]{compute_bounds_ats}} to calculate initial bounds and applies bootstrapping to estimate the standard deviation of these estimates, providing a summary in a data frame format.
#'
#' @param df A data frame containing the data.
#' @param d Name of the treatment variable in \code{df}.
#' @param m Name of the mediator variable in \code{df}.
#' @param y Name of the outcome variable in \code{df}.
#' @param cluster (Optional) The name of the cluster variable for clustered bootstrapping.
#' @param c_at_ratio (Optional) Specifies the ratio of E[Y(1,1) | C]/E[Y(1,1) | AT]. If this is specified, the direct effect for ATs is point-identified.
#' @param units A string denoting the units of the outcome variable (for labeling purposes).
#' @param numdraws The number of bootstrap draws for estimating the standard deviation.
#' @return A data frame summarizing the computed bounds with terms, estimates, and standard errors.
#' @examples
#' \dontrun{
#' data(example_data)
#' summarized_bounds <- lee_bounds(df = example_data, d = "treatment", m = "mediator", y = "outcome")
#' }
#' @export
#' @importFrom MedBounds compute_bounds_ats
#' @importFrom dplyr summarise_all
lee_bounds <- function(df, d, m, y, cluster = NULL, c_at_ratio = NULL, units = "", numdraws = 1000) {
  # Compute the point estimate using MedBounds::compute_bounds_ats
  pt_estimate <- MedBounds::compute_bounds_ats(
    df = df,
    d = d,
    m = m,
    y = y,
    c_at_ratio = c_at_ratio
  )
  
  # Compute bootstrap draws with optional clustering
  bootstrap_draws <- MedBounds:::compute_bootstrap_draws_clustered(
    df = df,
    d = d,
    m = m,
    y = y,
    f = function(..., c_at_ratio_) {
      MedBounds::compute_bounds_ats(..., c_at_ratio = c_at_ratio)
    },
    cluster = cluster,
    fix_n1 = FALSE,
    numdraws = numdraws
  )
  
  # Use dplyr to summarise and compute the standard deviation of the columns of bootstrap_draws
  bootstrap_sds <- bootstrap_draws %>%
    summarise_all(sd)
  
  # Determine the term list based on the presence of c_at_ratio
  termlist <- if (is.null(c_at_ratio)) {
    c("Lower bound", "Upper bound")
  } else {
    "Point estimate"
  }
  
  # Tidy the results into a data frame
  tidy_results <- data.frame(
    term = termlist,
    estimate = as.numeric(pt_estimate),
    std.error = as.numeric(bootstrap_sds),
    stringsAsFactors = FALSE
  )
  
  return(tidy_results)
}
