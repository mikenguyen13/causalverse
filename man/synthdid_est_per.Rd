% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synthdid_est_per.R
\name{synthdid_est_per}
\alias{synthdid_est_per}
\title{Estimate Treatment Effects for Each Period}
\usage{
synthdid_est_per(Y, N0, T0, weights)
}
\arguments{
\item{Y}{Data matrix with units as rows and time periods as columns.}

\item{N0}{Number of control units.}

\item{T0}{Number of pre-treatment periods.}

\item{weights}{Output from \code{synthdid}, containing lambda and omega weights.}
}
\value{
A list containing:
\itemize{
\item{est}{: TEs for each post-treatment period and cumulative ATEs.}
\item{y_obs}{: Observed outcomes for treated units.}
\item{y_pred}{: Predicted outcomes for treated units.}
\item{lambda.synth}{: Synthetic control lambda weights.}
\item{Ntr}{: Number of treated units.}
\item{Nco}{: Number of control units.}
}
}
\description{
Given the output from the \code{synthdid::synthdid_estimate} method, this function computes the
treatment effects (TEs) for each post-treatment period, along with the cumulative
average treatment effect (ATE). It also provides observed and predicted outcomes
for treated units, synthetic control weights, and counts of treated and control units.
}
\examples{
\dontrun{
library(tidyverse)
library(synthdid)
library(fixest)

setup <- base_did |>
  mutate(
    id = as.factor(id),
    period = as.integer(period),
    y = as.double(y),
    post = as.integer(post)
  ) |>
  # Correct treatment 
  dplyr::mutate(treatment = as.integer(if_else(treat == 0, 0, post))) |>
  synthdid::panel.matrices(unit = "id", time = "period", outcome = "y", treatment = "treatment") 

sdid <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)
synthdid_est_per(setup$Y, setup$N0, setup$T0, weights = attr(sdid, 'weights'))
}
}
