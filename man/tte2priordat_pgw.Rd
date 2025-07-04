% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tte2priordat_pgw.R
\name{tte2priordat_pgw}
\alias{tte2priordat_pgw}
\title{Prior and data setup for Bayesian Power generalized Weibull model fitting}
\usage{
tte2priordat_pgw(
  dat,
  scale.mean,
  scale.sd,
  shape.mean,
  shape.sd,
  powershape.mean,
  powershape.sd
)
}
\arguments{
\item{dat}{Matrix or data frame with time in the first column and event status in the second column.}

\item{scale.mean}{The a priori expected mean of the scale parameter.}

\item{scale.sd}{The priori expected standard deviation of the scale parameter.}

\item{shape.mean}{The priori expected mean of the shape parameter.}

\item{shape.sd}{The priori expected standard deviation of the shape parameter.}

\item{powershape.mean}{The priori expected mean of the power shape parameter.}

\item{powershape.sd}{The priori expected standard deviation of the power shape parameter.}
}
\description{
Setup of prior specifications and reformated time-to-event
data set as a list suitable for model fitting with \code{\link{bwsp_model}}.
}
\details{
The function converts time-to-event data and prior specifications for location ('.mean') and
precision ('.sd') of the power generalized Weibull parameters
to a suitable format for \code{rstan} model fitting
which is performed within \code{\link{bwsp_model}}.

Prior means suitable to reflect the prior belief can be found by plotting the
hazard and estimating the expected event time under different parameter combinations
using \code{\link{plot_pgw}} or \url{https://janoleko.shinyapps.io/pgwd/}.

Prior standard deviations should reflect the uncertainty about the prior belief
(i.e. set smaller standard deviation in case of high certainty about prior belief vs. larger
standard deviation in case of low certainty).
}
