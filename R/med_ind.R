#' Estimate Mediation Indirect Effects
#'
#' @description
#' `med_ind` estimates the indirect effects of an independent variable on a dependent variable 
#' through a mediator using Monte Carlo simulations (Selig & Preacher, 2008). It calculates the distribution of the product 
#' of path coefficients (a*b) and provides confidence intervals for the indirect effect, along 
#' with a ggplot histogram for visualization.
#'
#' @references
#' Selig, J. P., & Preacher, K. J. (2008, June). Monte Carlo method for assessing mediation: 
#' An interactive tool for creating confidence intervals for indirect effects [Computer software]. 
#' Available from http://quantpsy.org/.
#' 
#' @param a The regression coefficient for the effect of the independent (causal) variable on the mediator.
#' @param b The regression coefficient for the effect of the mediator on the dependent (outcome) variable.
#' @param var_a The variance of the coefficient a.
#' @param var_b The variance of the coefficient b.
#' @param cov_ab The covariance between coefficients a and b.
#' @param ci The confidence interval width for the indirect effect (default is 95 for a 95% CI).
#' @param iterations The number of iterations for the Monte Carlo simulation (default is 20000).
#' @param seed The seed for random number generation to ensure reproducibility (default is 1).
#' @param theme Custom theme that follows ggplots2 (default is AMA style)
#' @return A list containing the lower quantile, upper quantile, raw simulation data, and histogram plot of the indirect effects.
#' @examples
#' \dontrun{
#' result <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01)
#' result$lower_quantile
#' result$upper_quantile
#' result$plot
#' }
#' @export
med_ind <- function(a, b, var_a, var_b, cov_ab, ci = 95, iterations = 20000, seed = 1, theme = causalverse::ama_theme()) {
  set.seed(seed)
  
  acov_matrix <- matrix(c(var_a, cov_ab, cov_ab, var_b), 2, 2)
  
  sim_data <- MASS::mvrnorm(iterations, mu = c(a, b), Sigma = acov_matrix)
  ab_values <- sim_data[, 1] * sim_data[, 2]
  
  lower_q <- quantile(ab_values, (1 - ci/100)/2)
  upper_q <- quantile(ab_values, 1 - (1 - ci/100)/2)
  
  plot <- ggplot(data.frame(ab_values), aes(x = ab_values)) +
    geom_histogram(
      fill = "skyblue", color = "black"
      ) +
    labs(x = "Indirect Effect", y = "Frequency", title = "Distribution of Simulated Indirect Effects") +
    geom_vline(xintercept = lower_q, color = "red", linetype = "dashed") +
    geom_vline(xintercept = upper_q, color = "red", linetype = "dashed") +
    geom_vline(xintercept = 0, color = "black", linetype = "solid") +
    theme
  
  list(lower_quantile = lower_q, upper_quantile = upper_q, raw_data = ab_values, plot = plot)
}
