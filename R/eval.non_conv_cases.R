#' Evaluate number of non-convergence cases by tte and prior distribution
#'
#' Based on simulation study batch files the function groups the data by `tte.dist`
#' and `prior.dist`, and calculates the total planned and total not run repetitions,
#' and the proportion of repetitions that were not run
#' to guide the
#' tte and prior distributional choices (along with \code{\link{eval.execution_times}} 
#' and \code{\link{eval.eff_sample_sizes}}).
#'
#' @param pc_list a list containing simulation parameters (see \code{\link{sim.setup_simpars}})
#' 
#' @return A data frame with the following columns:
#'   - `tte.dist`: The time-to-event distribution as grouping factor.
#'   - `prior.dist`: The prior distribution as grouping factor.
#'   - `total.planned`: The total number of planned repetitions in this group.
#'   - `total.notrun`: The total number of repetitions that were not run in this group.
#'   - `prop.notrun`: The proportion of repetitions that were not run in this group.
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
  prog = sim.monitor.progress(pc_list)
  
  # add information about planned, run and not run repetitions to pc table
  pc_table = cbind(pc_table, reps.done = prog, reps.planned = reps) 
  pc_table = cbind(pc_table, reps.notrun = pc_table$reps.planned - pc_table$reps.done)
  
  
  # sum up numbers for groups depending on tte.dist, prior.dist
  nonconv.tab = dplyr::summarise(
    dplyr::group_by(pc_table, tte.dist, prior.dist),
    total.planned = sum(reps.planned),
    total.notrun = sum(reps.notrun),
    prop.notrun = total.notrun / total.planned,
    .groups = "drop"
  )
  
  return(nonconv.tab)
  
}



## END OF DOC