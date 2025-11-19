#' Merge result table batches from simulation study
#' 
#' Merges result table batches from simulation study obtained from using 
#' \code{\link{sim.run}} or \code{\link{sim.run_parallel}}.
#'
#' @param pc_list list of parameter combinations obtained from \link{sim.setup_simpars}
#' @param save if \code{TRUE} (default), merged table is saved as res_b.RData or res_f.RData
#' in same path where batches are stored; 
#' else, it is returned to global environment
#' @param bayes \code{TRUE} (default), results of Bayesian simulations are merged, 
#' else, results of frequentist simulations are merged
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

sim.merge_results = function(pc_list, save = T, bayes = T){
  
  
  
  
  if(bayes == T){
    sim.merge_results_b(pc_list = pc_list, save = save)
    
  }
  else if(bayes == F){
    sim.merge_results_f(pc_list = pc_list, save = save)
  }
}

## END OF DOC