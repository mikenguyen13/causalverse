% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ama_scale_color.R
\name{ama_scale_color}
\alias{ama_scale_color}
\title{Custom Color Scale for ggplot2: American Marketing Association Style}
\usage{
ama_scale_color(
  use_color = FALSE,
  palette_name = "OkabeIto",
  grayscale_limits = c(0.2, 0.8)
)
}
\arguments{
\item{use_color}{Logical. If TRUE, uses color, otherwise uses grayscale.}

\item{palette_name}{Character. Name of the color palette to use.}

\item{grayscale_limits}{Numeric vector. Limits for the grayscale gradient.}
}
\value{
A color scale for a ggplot2 plot.
}
\description{
This function provides a custom color scale for ggplot2 plots based on the guidelines set by the American Marketing Association.
}
\examples{
\dontrun{
library(ggplot2)
ggplot(mtcars, aes(mpg, wt, color = gear)) + geom_point(size = 4) + ama_scale_color()
}
}
