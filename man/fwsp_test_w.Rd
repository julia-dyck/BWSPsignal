% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fwsp_test_w.R
\name{fwsp_test_w}
\alias{fwsp_test_w}
\title{Frequentist Weibull Shape Parameter Test}
\usage{
fwsp_test_w(mod.output, credlevel = 1 - c(1:10/1000, 2:10/100))
}
\arguments{
\item{mod.output}{estimation output resulting from \code{fwsp_model(..., tte.dist = "w")}}

\item{credlevel}{vector of credibility levels for the tests to be performed}
}
\value{
A vector containing the test results for each credibility level.
}
\description{
Frequentist hypothesis test based on the shape parameter of the Weibull distribution.
}
\details{
The model estimate is a summary of a \code{survival::Survreg} outcome which provides
\eqn{ln(1 / \nu)} as transform of the shape parameter estimate.
The transform \eqn{ln(1 / \nu) = 0} under the null hypothesis \eqn{\nu = 1}.
The shape parameter test is performed on the transform leading the test outcome
equivalent to performing the test based on the shape parameter itself.
}
