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


# Define your estimator functions
mc_estimate = function(Y, N0, T0) {
  N1 = nrow(Y) - N0
  T1 = ncol(Y) - T0
  W       <- outer(c(rep(0, N0), rep(1, N1)), c(rep(0, T0), rep(1, T1)))
  mc_pred <- MCPanel::mcnnm_cv(Y, 1 - W, num_lam_L = 20)
  mc_fit  <- mc_pred$L + outer(mc_pred$u, mc_pred$v, '+')
  mc_est  <- sum(W * (Y - mc_fit)) / sum(W)
  return(mc_est)
}
mc_placebo_se = function(Y, N0, T0, replications = 200) {
  N1 = nrow(Y) - N0
  theta = function(ind) {
    mc_estimate(Y[ind, ], length(ind) - N1, T0)
  }
  
  res <- sqrt((replications - 1) / replications) * sd(replicate(replications, theta(sample(1:N0))))
  return(res)
}

difp_estimate = function(Y, N0, T0) {
  synthdid::synthdid_estimate(Y,
                              N0,
                              T0,
                              weights = list(lambda = rep(1 / T0, T0)),
                              eta.omega = 1e-6)
}

sc_estimate_ridge = function(Y, N0, T0) {
  synthdid::sc_estimate(Y, N0, T0, eta.omega = ((nrow(Y) - N0) * (ncol(Y) -
                                                                    T0)) ^ (1 / 4))
}
difp_estimate_ridge = function(Y, N0, T0) {
  synthdid::synthdid_estimate(Y, N0, T0, weights = list(lambda = rep(1 / T0, T0)))
}

panel_estimators = list(
  synthdid   = synthdid::synthdid_estimate,
  did        = synthdid::did_estimate,
  sc         = synthdid::sc_estimate,
  sdid       = synthdid::synthdid_estimate,
  difp       = difp_estimate,
  mc         = mc_estimate, 
  sc_ridge   = sc_estimate_ridge,
  difp_ridge = difp_estimate_ridge
)