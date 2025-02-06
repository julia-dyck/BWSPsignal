#' merge result table batches from simulationstudy
#'
#' @param pc_table table of scenarios
#' @param wd_load path in which the result batches are stored
#' @param wd_save path in which the merged result table will be stored
#' @param nr_batches number of batch files per scenario
#' @param save logical indicating whether to automatically save the merged result in
#' \code{wd_save} or just return it the session environment (by default \code{FALSE}) 
#'
#' @return one result dataframe containing all simulations (one per row); the 
#' pc specifications are in the first 8 columns, remaining specifications contain
#' statistics on prior specifications and posterior sample
#'
#'
#' @export


sim.merge.results = function(pc_table,
                            wd_load,
                            wd_save,
                            nr_batches = 10, 
                            save = F){
  # prepare output matrix format
  load_for_dims = sim.load.scenario(wd = wd_load, pc = pc_table[1,], batchnr = 1)
  ncol_out = ncol(load_for_dims) # nr of cols of the merged results table
  nr_scenarios = nrow(pc_table)
  merged_res = matrix(nrow = 0, ncol = ncol_out)

  for(i in 1:nr_scenarios){
    for(j in 1:nr_batches){
      merged_res = rbind(merged_res, sim.load.scenario(wd = wd_load, pc = pc_table[i,], batchnr = j))
    }
  }
  merged_res = data.frame(merged_res)
  if(save == T){
    save(merged_res, file = paste0(wd_save, "/merged_res.RData"))
  }
  else{
    return(merged_res)
  }

}

