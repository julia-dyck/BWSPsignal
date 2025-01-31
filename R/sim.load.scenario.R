#' Load simulated scenario
#' 
#' Load all simulations of a simulated scenario and return number of run 
#' simulations.
#'
#' @param wd working directory were ALL the simulation files lie
#' 
#' @param pc parameter combination (numeric version)
#' 
#' @return Returns a table with counts on run simulation scenarios
#' 
#' @export
#'
#'
sim.load.scenario = function(wd, pc, batchnr = 1){
  N = pc[1]
  br = pc[2]
  adr.rate = pc[3]
  adr.when = pc[4]
  adr.relsd = pc[5]
  study.period = pc[6]
  dist.ass = pc[7]
  adr.ass = pc[8]
  # load the first batch
  filename = paste(
                   N, br, adr.rate, adr.when, adr.relsd,
                   study.period, dist.ass, adr.ass,
                   "bADR_sim", batchnr,".RData", sep = "_")
  path = paste0(wd, "/", filename)
  #return(path)
  load(file = path)
  res = res.batch
  return(res)
}

#load_scenario(wd = "C:/Users/jdyck/sciebo/bADR_simstudyres01",
#              pc = c(500, 0.1, 0.5, 0.5, 0.05, 365, 1,1), batchnr = 1)





