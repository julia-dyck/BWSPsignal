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
#'       
#'       
#' @export

sim.merge_results = function(pc_list, save = T, bayes = T){
  
  ## argument checks -----------------------------------------------------------
  # argument check for pc_list
  pc_list_is_valid <-
    is.list(pc_list) &&
    # pc_list$dgp must be a data.frame
    !is.null(pc_list$dgp) && 
    is.data.frame(pc_list$dgp) &&
    # pc_list$fit must be a list whose elements are all data.frames
    !is.null(pc_list$fit) && 
    is.list(pc_list$fit) &&
    length(pc_list$fit) > 0 &&
    all(vapply(pc_list$fit, is.data.frame, logical(1))) &&
    # pc_list$test must be a list
    !is.null(pc_list$test) && 
    is.list(pc_list$test) &&
    # pc_list$add must be a list with required numeric/character elements
    !is.null(pc_list$add) &&
    is.list(pc_list$add) &&
    is.numeric(pc_list$add$reps) &&
    is.numeric(pc_list$add$batch.size) &&
    is.numeric(pc_list$add$batch.nr) &&
    is.character(pc_list$add$resultpath) &&
    is.numeric(pc_list$add$stanmod.chains) &&
    is.numeric(pc_list$add$stanmod.iter) &&
    is.numeric(pc_list$add$stanmod.warmup) &&
    # pc_list$pc_table must be a non-empty data.frame
    !is.null(pc_list$pc_table) &&
    is.data.frame(pc_list$pc_table)
  
  if (!pc_list_is_valid) {
    stop("Argument pc_list has wrong format. It must be a list produced by sim.setup_sim_pars().\n")
  }
  
  # argument check for save
  if (!is.logical(save) || length(save) != 1 || is.na(save)) {
    stop("Argument 'save' must be a single logical value (TRUE or FALSE).\n")
  }
  
  # argument check for bayes
  if (!is.logical(bayes) || length(bayes) != 1 || is.na(bayes)) {
    stop("Argument 'bayes' must be a single logical value (TRUE or FALSE).\n")
  }
  
  ## fct body ------------------------------------------------------------------
  if(bayes == T){
    sim.merge_results_b(pc_list = pc_list, save = save)
    
  }
  else if(bayes == F){
    sim.merge_results_f(pc_list = pc_list, save = save)
  }
}

## END OF DOC