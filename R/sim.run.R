#' run all simulation scenarios
#' 
#' @param pc_list list of parameter combinations obtained from \link{\code{sim.setup_sim_pars}}
#' 
#' Runs simulations for all data generating processes, model and test alternatives
#' specified in pc_list. If part of the simulations is already run and saved in the 
#' detemined resultpath, only missing simulations are run and saved.
#' 
#' 
#' 
#' @export
#' 
#' 
sim.run = function(pc_list){
  for(ind.dgp in 1:nrow(pc_list$dgp)){      # go through dgp scenarios (per row)
    if(nrow(pc_list$fit$w)>0){
      for(ind.fitw in 1:nrow(pc_list$fit$w)){ # go through weibull fitting parameter combis
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$w[ind.fitw,c("tte.dist", "prior.dist", "prior.belief")])
        
        for(ind.batch in 1:pc_list$add$batch.nr){
          
          tryCatch({
            # check whether batch file already exists
            sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch)
            message("File exists.")
          },
          error=function(cond) {
            # if not, generate result batch file
            # repeat modelling for one scenario
            sim.repeat.1.scenario(pc = pc_vect,
                                  pc_list = pc_list,
                                  batch.ind = ind.batch
            )
          }
          )
          
        }
      }
    }
    
    if(nrow(pc_list$fit$dw)>0){
      for(ind.fitdw in 1:nrow(pc_list$fit$dw)){
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$dw[ind.fitdw,c("tte.dist", "prior.dist", "prior.belief")])
        # repeat modelling for one scenario
        for(ind.batch in 1:pc_list$add$batch.nr){
          tryCatch({
            # check whether batch file already exists
            sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch)
            message("File exists.")
          },
          error=function(cond) {
            # if not, generate result batch file
            sim.repeat.1.scenario(pc = pc_vect,
                                  pc_list = pc_list,
                                  batch.ind = ind.batch
            )
          }
          )
        }
      }
    }
    
    if(nrow(pc_list$fit$pgw)>0){
      for(ind.fitpgw in 1:nrow(pc_list$fit$pgw)){
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$pgw[ind.fitpgw,c("tte.dist", "prior.dist", "prior.belief")])
        # repeat modelling for one scenario
        for(ind.batch in 1:pc_list$add$batch.nr){
          tryCatch({
            # check whether batch file already exists
            sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch)
            message("File exists.")
          },
          error=function(cond) {
            # if not, generate result batch file
            sim.repeat.1.scenario(pc = pc_vect,
                                  pc_list = pc_list,
                                  batch.ind = ind.batch
            )
          }
          )
        }
      }
    }
  }
}


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
#' Simulation runs can be parallelized using the \code{future} package.
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
  pc_table_as_list = base::split(pc_table_ext, seq_len(nrow(pc_table_ext)))
  
  # Parallelized execution using furrr::future_walk
  furrr::future_walk(
    .x = pc_table_as_list,
    .f = function(row) {
      pc_vect = row[1:9]
      ind.batch = row$batch_nr
      
      base::tryCatch({
        sim.load.scenario(pc = pc_vect, wd = pc_list$add$resultpath, batchnr = ind.batch)
        base::message("File exists.")
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