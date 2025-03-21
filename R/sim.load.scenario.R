#' Load simulated scenario
#' 
#' Load all simulations of a simulated scenario (obtained using 
#' \code{\link{sim.repeat.1.scenario}}) and return number of run 
#' simulations.
#'
#' @param wd working directory were ALL the simulation files lie
#' 
#' @param pc parameter combination (numeric version)
#' 
#' @param batchnr nr of files (batches) containing simulation output based on the 
#' same pc
#' 
#' @return Returns a table with counts on run simulation scenarios
#' 
#' @export
#'
#'

sim.load.scenario = function(wd, pc, batchnr = 1){

  filename = paste(c(pc, "bADR_sim", batchnr, ".RData") ,collapse="_")
  
  path = paste0(wd, "/", filename)
  
  # return(path)
  load(file = path)
  res = res.batch
  return(res)
}





## END OF DOC