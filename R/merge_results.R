
#' @export

# merge result table batches from simstudy01

## input arguments
# ## pc_table: table of scenarios pc_table
# ## wd_load: path in which the result batches are stored
# ## wd_save: path in which the merged result table will be stored
# ## nr_batches: number of batch files per scenario

## output
# one result table containing all simulations:
# ## one row = one simulation run
# ## there are 100 rows per scenario, the scenario prespecifications
#    are in the first 8 cols of the table

merge_results = function(pc_table,
                            wd_load,
                            wd_save,
                            nr_batches = 10, save = F){
  # prepare output matrix format
  load_for_dims = load_scenario(wd = wd_load, pc = pc_table[1,], batchnr = 1)
  ncol_out = ncol(load_for_dims) # nr of cols of the merged results table
  nr_scenarios = nrow(pc_table)
  merged_res = matrix(nrow = 0, ncol = ncol_out)

  for(i in 1:nr_scenarios){
    for(j in 1:nr_batches){
      merged_res = rbind(merged_res, load_scenario(wd = wd_load, pc = pc_table[i,], batchnr = j))
    }
  }
  merged_res = data.frame(merged.res)
  if(save == T){
    save(merged_res, file = paste0(wd_save, "/merged_res.RData"))
  }
  else{
    return(merged_res)
  }

}

#test = merge_results(pc[1:2,])
