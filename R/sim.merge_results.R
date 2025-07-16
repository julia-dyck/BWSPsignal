#' Merge result table batches from simulation study
#' 
#' Merges result table batches from simulation study obtained from using 
#' \code{\link{sim.run}}.
#'
#' @param pc_list list of parameter combinations obtained from \link{sim.setup_simpars}
#' @param save if \code{TRUE} (default), merged table is saved in same path where batches are stored; 
#' else, it is returned to global environment
#' 
#' @return Dataframe containing all simulation results (one repetition of one 
#' simulation scenario per row). The 
#' simulation parameters are stored in the first 9 columns. The remaining columns 
#' contain 
#' \enumerate{
#'       \item posterior summary statistics and percentiles (ie information on the 
#'       posterior distribution) for each shape parameter and 
#'       \item posterior credibility 
#'       intervals (as specified in \code{$test} list element obtained from \link{sim.setup_simpars}).
#'       }
#' @export

sim.merge_results = function(pc_list, save = T, framework = c("bwsp", "fwsp")){
  
  framework = match.arg(framework)
  
  if(framework == "bwsp"){
    sim.merge_results_b(pc_list = pc_list, save = save)
  }
  else if(framework == "fwsp"){
    sim.merge_results_f(pc_list = pc_list, save = save)
  }
}

## END OF DOC