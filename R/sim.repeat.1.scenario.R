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



sim.repeat.1.scenario = function(pc, pc_list, batch.ind) {
  batch.size = pc_list$add$batch.size
  
  # Run simulations, capturing NULLs from errors
  res.batch.list = replicate(batch.size,
                             sim.fit.to.1.sample(pc = pc, pc_list = pc_list),
                             simplify = FALSE)
  
  ## bstats
  res.batch = do.call(rbind, lapply(res.batch.list, function(x) x$bstats))
  # Convert list-columns to atomic vectors (flatten the structure)
  res.batch = as.data.frame(lapply(res.batch, unlist), stringsAsFactors = FALSE)
  
  # Save bstats batch
  path = pc_list$add$resultpath
  filename = paste(c(pc, "bADR_sim", batch.ind, ".RData"), collapse = "_")
  save(res.batch, file = file.path(path, filename))
  
  ## ftests 
  res.batch = do.call(rbind, lapply(res.batch.list, function(x) x$ftests))
  # Convert list-columns to atomic vectors (flatten the structure)
  res.batch = data.frame(res.batch)
  res.batch = lapply(res.batch, unlist)
  res.batch = data.frame(res.batch)
  
  # Save ftests batch
  path = pc_list$add$resultpath
  filename = paste(c(pc, "fADR_sim", batch.ind, ".RData"), collapse = "_")
  save(res.batch, file = file.path(path, filename))
}

# # testing
# repeated.runs = sim.repeat.1.scenario(pc = pc, pc_list = pc_list, batch.ind = 1, save = F)


## END OF DOC
