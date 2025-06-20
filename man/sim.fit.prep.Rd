% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.fit.prep.R
\name{sim.fit.prep}
\alias{sim.fit.prep}
\title{Prepare Bayesian pgW fitting for simulation study}
\usage{
sim.fit.prep(ttedat, pc, pc_list)
}
\arguments{
\item{ttedat}{time-to-event data set}

\item{pc}{vector representing one parameter combination used in simulation study}

\item{pc_list}{list containing all parameter combinations and additional
information necessary for simulation study}
}
\value{
A list containing all information to be inserted as \code{datstan}
argument into the \code{\link{bwsp_model}} function.
}
\description{
Formatting of simulated data generated with \code{\link{datagen_tte}} as
building block for tuning the Bayesian WSP test via simulation study.
}
