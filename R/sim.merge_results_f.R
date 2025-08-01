#' Merge frequentist result table batches from simulation study
#' 
#' Merges result table batches from simulation study (frequentist part) obtained from using 
#' \code{\link{sim.run}}.
#'
#' @param pc_list list of parameter combinations obtained from \link{sim.setup_simpars}
#' @param save if \code{TRUE} (default), merged table is saved in same path where batches are stored; 
#' else, it is returned to global environment
#' 
#' @return Dataframe containing all simulation results (one repetition of one 
#' simulation scenario per row). The 
#' simulation parameters are stored in the first 9 columns. The remaining columns 
#' contain 
#' \enumerate{
#'       \item posterior summary statistics and percentiles (ie information on the 
#'       posterior distribution) for each shape parameter and 
#'       \item posterior credibility 
#'       intervals (as specified in \code{$test} list element obtained from \link{sim.setup_simpars}).
#'       }
#' 

sim.merge_results_f = function(pc_list, save = T){
  
  pc_table = pc_list$pc_table
  pc_table_ext = dplyr::cross_join(pc_table, data.frame(batch_nr = 1:pc_list$add$batch.nr))
  
  res_f = data.frame(matrix(nrow = 0, ncol = ncol(pc_list$pc_table) + 3*length(pc_list$input$cred.level)))
  colnames(res_f) = c(colnames(pc_list$pc_table), 
                      paste0("fwsp_", rep(c("w", "dw", "pgw"), each = length(pc_list$input$cred.level)), "_", rep(pc_list$input$cred.level, times = 3))
  )

  for(ind in 1:nrow(pc_table_ext)){
    pc_vect = pc_table_ext[ind, 1:9]
    ind.batch = pc_table_ext[ind, 10]
    
    tryCatch({
      batch = sim.load.scenario(pc = pc_vect, wd = pc_list$add$resultpath, batchnr = ind.batch, bayes = F)
      res_f = rbind(res_f, batch)
      message("File exists.")
    },
    error = function(cond) {
      message("File does not exist.")
    })
  }
  
  if(save == T){
    # save result
    path = pc_list$add$resultpath
    filename = "res_f.RData"
    save(res_f, file=paste0(path, "/", filename))
  }
  if(save == F){
    return(res_f)
  }
}

## END OF DOC