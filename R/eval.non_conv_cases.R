#### investigate number of non-nonvergence cases in simulation
#'
#' This function processes a list containing simulation data and computes
#' the total number of repetitions that were completed and not completed
#' based on the progress of the simulation. It groups the data by `tte.dist`
#' and `prior.dist`, calculates the total planned and total not run repetitions,
#' and computes the proportion of repetitions that were not run.
#'
#' @param pc_list A list generated with \code{\link{sim.setup_simpars}} after 
#' simulations were run.
#' 
#' @return A data frame with the following columns:
#'   - `tte.dist`: The time-to-event distribution.
#'   - `prior.dist`: The prior distribution.
#'   - `total.notrun`: The total number of repetitions that were not run.
#'   - `total.planned`: The total number of planned repetitions.
#'   - `prop.notrun`: The proportion of repetitions that were not run.
#'
#' 
#' @export



eval.non_conv_cases = function(pc_list){
  # shorten object names
  pc_table = pc_list$pc_table
  reps = pc_list$add$reps
  batch.size = pc_list$add$batch.size
  batch.nr = pc_list$add$batch.nr
  
  # monitor progress: how many batches were successfully run/ not run in simulation?
  prog = sim.monitor.progress(pc_table,
                              wd = pc_list$add$resultpath,
                              batch_max = pc_list$add$batch.nr)
  
  # add information about planned, run and not run repetitions to pc table
  pc_table = cbind(pc_table, reps.done = prog*batch.size, reps.planned = reps) 
  pc_table = cbind(pc_table, reps.notrun = pc_table$reps.planned - pc_table$reps.done)
  
  
  # sum up numbers for groups depending on tte.dist, prior.dist
  nonconv.tab = dplyr::summarise(
    dplyr::group_by(pc_table, tte.dist, prior.dist),
    total.notrun = sum(reps.notrun),
    total.planned = sum(reps.planned),
    prop.notrun = total.notrun / total.planned,
    .groups = "drop"
  )
  
  return(nonconv.tab)
  
}



## END OF DOC