% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_CSS.R
\name{plot_css}
\alias{plot_css}
\title{Plot community size spectrum}
\usage{
plot_css(data, normalised = T, logbase = 2)
}
\arguments{
\item{data}{Table of mass, normalised density, and density}

\item{normalised}{Logical option of normalised (TRUE) or non-normalised (FALSE)}

\item{logbase}{The chosen logbase for the plotting}
}
\value{
A ggplot figure
}
\description{
This function plots an abundance size spectrum, with the option for
normalised or non-normalised axes
}
