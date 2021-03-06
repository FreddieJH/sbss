% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_SSD_params.R
\name{est_ssd}
\alias{est_ssd}
\title{Estimate species size distribution (SSD)}
\usage{
est_ssd(
  df,
  lmax_colname = "lmax",
  a_colname = "a",
  b_colname = "b",
  meanlog_eqn,
  sdlog_eqn
)
}
\arguments{
\item{df}{A tibble or dataframe with columns of asymptotic size, and length-weight estimates (a, b)}

\item{lmax_colname}{character, the name of the column containing the asymptotic length of the species}

\item{a_colname}{character, the name of the column containing the species length weight parameter a}

\item{b_colname}{character, the name of the column containing the species length weight parameter b}

\item{meanlog_eqn}{function, the relationship between asymptotic mass and meanlog}

\item{sdlog_eqn}{function, the relationship between asymptotic mass and sdlog}
}
\value{
A tibble of meanlog and sdlog parameters of SSD
}
\description{
This function estimates a species size distribution based on
parameters from the species asymptotic mass and length-weight relationship;
\eqn{W = a * L^b}.
}
