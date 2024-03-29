% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/med_ind.R
\name{med_ind}
\alias{med_ind}
\title{Estimate Mediation Indirect Effects}
\usage{
med_ind(
  a,
  b,
  var_a,
  var_b,
  cov_ab,
  ci = 95,
  iterations = 20000,
  seed = 1,
  theme = causalverse::ama_theme()
)
}
\arguments{
\item{a}{The regression coefficient for the effect of the independent (causal) variable on the mediator.}

\item{b}{The regression coefficient for the effect of the mediator on the dependent (outcome) variable.}

\item{var_a}{The variance of the coefficient a.}

\item{var_b}{The variance of the coefficient b.}

\item{cov_ab}{The covariance between coefficients a and b.}

\item{ci}{The confidence interval width for the indirect effect (default is 95 for a 95\% CI).}

\item{iterations}{The number of iterations for the Monte Carlo simulation (default is 20000).}

\item{seed}{The seed for random number generation to ensure reproducibility (default is 1).}

\item{theme}{Custom theme that follows ggplots2 (default is AMA style)}
}
\value{
A list containing the lower quantile, upper quantile, raw simulation data, and histogram plot of the indirect effects.
}
\description{
\code{med_ind} estimates the indirect effects of an independent variable on a dependent variable
through a mediator using Monte Carlo simulations (Selig & Preacher, 2008). It calculates the distribution of the product
of path coefficients (a*b) and provides confidence intervals for the indirect effect, along
with a ggplot histogram for visualization.
}
\examples{
\dontrun{
result <- med_ind(a = 0.5, b = 0.7, var_a = 0.04, var_b = 0.05, cov_ab = 0.01)
result$lower_quantile
result$upper_quantile
result$plot
}
}
\references{
Selig, J. P., & Preacher, K. J. (2008, June). Monte Carlo method for assessing mediation:
An interactive tool for creating confidence intervals for indirect effects \link{Computer software}.
Available from http://quantpsy.org/.
}
