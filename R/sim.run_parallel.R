#' run all or subset of simulation scenarios
#' 
#' @param pc_list list of parameter combinations obtained from \link{\code{sim.setup_sim_pars}}
#' @param subset_ind vector of integers specifying which rows of \code{pc_list$pc_table} 
#' to be considered in simulation runs
#' 
#' 
#' Runs simulations for all data generating processes, model and test alternatives
#' specified in pc_list. If part of the simulations is already run and saved in the 
#' detemined resultpath, only missing simulations are run and saved.
#' 
#' Simulation runs can be parallelized using the \link[future] package.
#' 
#' @example 
#'  \dontrun{
#'  
#'  
#'  # install.packages(future)
#'  future::plan(multisession) # or another plan strategy
#'  sim.run_parallel(pc_list, subset_ind = NULL) # run all simulations
#'  
#'  
#'  # To run only a subset of simulation scenarios, specify the indices of the rows
#'  # in pc_list$pc_table that you want to run. For example, to run the first 10 scenarios:
#'  
#'  # install.packages(future)
#'  future::plan(multisession) # or another plan strategy
#'  sim.run_parallel(pc_list, subset_ind = 1:10) # run first 10 simulation scenarios
#' 
#' }
#' 
#' 
#' 
#' 
#' @export
#' 
#' 


sim.run_parallel = function(pc_list, subset_ind = NULL) {
  
  if (!is.null(subset_ind)) {
    pc_table = pc_list$pc_table[subset_ind, ]
  } else {
    pc_table = pc_list$pc_table
  }
  
  pc_table_ext = dplyr::cross_join(pc_table, data.frame(batch_nr = 1:pc_list$add$batch.nr))
  
  # split table rows into list elements
  pc_table_as_list = split(pc_table_ext, seq_len(nrow(pc_table_ext)))
  
  # Parallelized execution using furrr::future_walk
  furrr::future_walk(
    .x = pc_table_as_list,
    .f = function(row) {
      pc_vect = row[1:9]
      ind.batch = row$batch_nr
      
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
    },
    .options = furrr::furrr_options(seed = NULL)  # optional seed if reproducibility isn't critical
  )
}



## END OF DOC