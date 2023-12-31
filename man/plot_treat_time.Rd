% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_treat_time.R
\name{plot_treat_time}
\alias{plot_treat_time}
\title{Plot number of treated units over time or return a dataframe.}
\usage{
plot_treat_time(
  data,
  time_var,
  unit_treat,
  outlier_method = "iqr",
  show_legend = FALSE,
  theme_use = causalverse::ama_theme(),
  legend_title = "Point Type",
  legend_labels = c("Regular", "Outlier"),
  regular_size = 3,
  outlier_size = 5,
  regular_color = "black",
  outlier_color = "red",
  regular_shape = 16,
  outlier_shape = 17,
  title = "Random Time Assignment",
  xlab = "Time",
  ylab = "Number of Treated Units",
  output = "plot",
  ...
)
}
\arguments{
\item{data}{Dataframe containing data.}

\item{time_var}{Time variable for aggregating the number of treated units.}

\item{unit_treat}{Variable indicating if the unit was treated in a specific time period.}

\item{outlier_method}{Method for outlier detection ("iqr" or "z-score").}

\item{show_legend}{Logical indicating whether to show legend.}

\item{theme_use}{ggplot2 theme to use.}

\item{legend_title}{Title for legend.}

\item{legend_labels}{Labels for regular and outlier points.}

\item{regular_size}{Size of regular points.}

\item{outlier_size}{Size of outlier points.}

\item{regular_color}{Color of regular points.}

\item{outlier_color}{Color of outlier points.}

\item{regular_shape}{Shape of regular points.}

\item{outlier_shape}{Shape of outlier points.}

\item{title}{Plot title.}

\item{xlab}{X-axis label.}

\item{ylab}{Y-axis label.}

\item{output}{Type of output ("plot" or "dataframe").}

\item{...}{Additional arguments to pass to ggplot2::labs.}
}
\value{
ggplot2 object or dataframe.
}
\description{
Plot number of treated units over time or return a dataframe.
}
\examples{
# Example usage:
\dontrun{
data <- data.frame(time = c(1,1,2,2,3,3), treat = c(0,1,1,1,0,0))
plot_treat_time(data, time_var = time, unit_treat = treat)
plot_treat_time(data, time_var = time, unit_treat = treat, output = "dataframe")
}
}
