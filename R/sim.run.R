#' run all simulation scenarios
#' 
#' @param pc_list list of parameter combinations obtained from \link{\code{sim.setup_sim_pars}}
#' @param subset_ind vector of integers specifying which rows of \code{pc_list$pc_table} 
#' to be considered in simulation runs
#' 
#' Runs simulations for all data generating processes, model and test alternatives
#' specified in pc_list. If part of the simulations is already run and saved in the 
#' detemined resultpath, only missing simulations are run and saved.
#' 
#' #' @example 
#'  \dontrun{
#'  
#'  
#'  
#'  sim.run(pc_list, subset_ind = NULL) # run all simulations
#'  
#'  
#'  # To run only a subset of simulation scenarios, specify the indices of the rows
#'  # in pc_list$pc_table that you want to run. For example, to run the first 10 scenarios:
#'  
#'  sim.run(pc_list, subset_ind = 1:10) # run first 10 simulation scenarios
#' 
#' }
#' 
#' 
#' @export
#' 
#' 

sim.run = function(pc_list, subset_ind = NULL){
  if (!is.null(subset_ind)) {
    pc_table = pc_list$pc_table[subset_ind, ]
  } else {
    pc_table = pc_list$pc_table
  }
  
  pc_table_ext = dplyr::cross_join(pc_table, data.frame(batch_nr = 1:pc_list$add$batch.nr))
  
  for(ind in 1:nrow(pc_table_ext)){
    pc_vect = pc_table_ext[ind, 1:9]
    ind.batch = pc_table_ext[ind, 10]
    
    tryCatch({
      sim.load.scenario(pc = pc_vect, wd = pc_list$add$resultpath, batchnr = ind.batch)
      message("File exists.")
    },
    error = function(cond) {
      sim.repeat.1.scenario(
        pc = pc_vect,
        pc_list = pc_list,
        batch.ind = ind.batch
      )
    })
  }
  
}


## END OF DOC