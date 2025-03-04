#' repeat simulation for one sample scenario (pc) in simulationstudy
#'
#' Repeat the data generation and fitting and results output for
#' one scenario parameter combination (pc).
#'
#' @param batch.ind batch index to save them under different names and 
#' prevent overwriting
#' @param reps.per.batch number of repeated simulations for one scenario forming one batch
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



sim.repeat.1.scenario = function(batch.ind, batch.size, pc,
                                 save = T, path = getwd()){

  res.batch = t(replicate(batch.size,
                          sim.fit.to.1.sample(pc = pc),
                          simplify = T)
                )

  # save result
  if(save == T){
    filename = paste(c(pc, "bADR_sim", batch.ind, ".RData") ,collapse="_")
    save(res.batch, file=paste0(path, "/", filename))
  }

  if(save == F){
    return(res.batch)
  }
}

## END OF DOC
