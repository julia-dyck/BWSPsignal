% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.run_parallel.R
\name{sim.run_parallel}
\alias{sim.run_parallel}
\title{run all or subset of simulation scenarios}
\usage{
sim.run_parallel(pc_list, subset_ind = NULL)
}
\arguments{
\item{pc_list}{list of parameter combinations obtained from \link{\code{sim.setup_sim_pars}}}

\item{subset_ind}{vector of integers specifying which rows of \code{pc_list$pc_table}
to be considered in simulation runs

Runs simulations for all data generating processes, model and test alternatives
specified in pc_list. If part of the simulations is already run and saved in the
detemined resultpath, only missing simulations are run and saved.

Simulation runs can be parallelized using the \link\link{future} package.}
}
\description{
run all or subset of simulation scenarios
}
