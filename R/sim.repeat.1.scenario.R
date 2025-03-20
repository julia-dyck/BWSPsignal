#' repeat simulation for one sample scenario (pc) in simulationstudy
#'
#' Repeat the data generation and fitting and results output for
#' one scenario parameter combination (pc).
#'
#' @param pc vector containing the parameter combination for data generation and model fitting, namely:
#' - N: sample size
#' - br: background rate
#' - adr.rate: adverse event rate
#' - adr.when: adverse event time
#' - adr.relsd: relative standard deviation of the adverse event time
#' - study.period: study period
#' - tte.dist: time-to-event distribution
#' - prior.dist: prior distribution
#' - prior.belief: prior belief
#' @param pc_list list containing all parameter combinations for the simulation study
#'                (necessary to extract further parameters for simulation study specification)
#' @param batch.ind integer indicating the pc batch index (for saving purposes)
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



sim.repeat.1.scenario = function(pc, pc_list, batch.ind, save = T){
  batch.size = pc_list$add$batch.size
  
  
  res.batch = t(replicate(batch.size,
                          sim.fit.to.1.sample(pc = pc,
                                              pc_list = pc_list),
                          simplify = T)
                )
  res.batch = data.frame(res.batch)
  
  if(save == T){
    # save result
    filename = paste(c(pc, "bADR_sim", batch.ind, ".RData") ,collapse="_")
    save(res.batch, file=paste0(path, "/", filename))
  }
  else{ # option for testing/developing
    return(res.batch)
  }
}





## END OF DOC
