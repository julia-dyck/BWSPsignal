#' repeat simulation for one sample scenario (pc) in simulationstudy
#'
#' Repeat the data generation and fitting and results output for
#' one scenario parameter combination (pc).
#'
#' @param batch.nr number of batch to save them under different names and 
#' prevent overwriting
#' @param reps number of repeated simulations for one scenario forming one batch
#' @param pc matrix of parameter combinations for the scenario to be simulated
#' @param save logical: states whether output is to be saved in a specified file
#'        (T) or in the global environment (F)
#' @param path file path specifying where to save the output if save = T
#'
#' @return a data frame containing all fitting and posterior sample statistics of 
#' the simulation runs (statistics are gathered using the 
#' \code{\link{sim.stanfit.to.fitstats}} and \code{\link{sim.stanfit.to.poststats}} 
#' functions).
#'
#'
#' @export
#'
#'



sim.repeat.1.scenario = function(batch.nr, reps = 10, pc,
                                 save = F, path = getwd()){
  # batch.nr = number of batch to save them under different names and prevent overwriting
  # reps = number of repetitions for this scenario
  # save = whether output is to be saved in a specified file (T) or in the global environment (F)
  # path = where to save the output if save = T
  #------------------------------

  res.batch = t(replicate(reps,
                          sim.fit.to.1.sample(pc = pc),
                          simplify = T)
                )

  # save result
  if(save == T){
    filename = paste(c(pc, "bADR_sim", batch.nr, ".RData") ,collapse="_")
    save(res.batch, file=paste0(path, "/", filename))
  }

  if(save == F){
    return(res.batch)
  }
}

## END OF DOC
