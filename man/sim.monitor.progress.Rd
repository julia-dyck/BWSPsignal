% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.monitor.progress.R
\name{sim.monitor.progress}
\alias{sim.monitor.progress}
\title{Monitor the progress of simulationstudy}
\usage{
sim.monitor.progress(pc_list)
}
\arguments{
\item{pc_list}{simulation study parameters; mainly used here are pc_list$pc_table,
a data frame with all simulation scenarios and pc_list$add$reps stating the planned
number of repetitions per scenario.}
}
\value{
a vector with the number of successfully run simulations for each pc
}
\description{
The function monitors the progress of the simulation study by going through
raw result files.
This can be done to find the restarting point when the running of simulations
was interrupted or to investigate which simulations could not be run
successfully (due to convergence issues).
}
