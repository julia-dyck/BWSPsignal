#' @title merge result table batches from simulation study
#' 
#' @description Merges result table batches from simulation study obtained from using 
#' \code{\link{sim.run}}.
#'
#' @param pc_list list of parameter combinations obtained from \code{\link{sim.setup_simpars}}
#' @param save if \code{TRUE} (default), merged table is saved in same path where batches are stored; 
#' else, it is returned to global environment
#' 
#' @return Dataframe containing all simulation results (one repetition of one 
#' simulation scenario per row). The 
#' simulation parameters are stored in the first 9 columns. The remaining columns 
#' contain 
#' 
#' * posterior summary statistics and percentiles (ie information on the 
#' posterior distribution) for each shape parameter and 
#' 
#' * posterior credibility 
#' intervals (as specified in \code{$test} list element obtained from \code{\link{sim.setup_simpars}).
#'
#'
#' @export

sim.merge_results = function(pc_list, save = T){
  # prepare ncols of merged df per tte.dist
  ncols_parvect = 10
  ncols_post.summary = 5 # per shape parameter
  ncols_eti = 2*length(pc_list$input$cred.level) # per shape parameter
  ncols_hdi = 2*length(pc_list$input$cred.level) # per shape parameter
  ncols_per = 101 # per shape parameter
  ncols_per.shape = ncols_post.summary + ncols_eti + ncols_hdi +  ncols_per
  
  # for w
  ncols = ncols_parvect + 1*ncols_per.shape
  # setup empty df
  merged.res.w =  data.frame()
  
  # adjust ncols of df (no. shapes depends on tte.dist)
  ncols = ncols_parvect + 2*ncols_per.shape # 2 = length(nu1, nu2)
  # setup empty df
  merged.res.dw =  data.frame()
  
  # for pgw
  ncols = ncols_parvect + 2*ncols_per.shape # 2 = length(nu, ga)
  # setup empty df
  merged.res.pgw = data.frame()
  
  # go through all pc combinations in pc_list:
  
  for(ind.dgp in 1:nrow(pc_list$dgp)){      # go through dgp scenarios (per row)
    if(nrow(pc_list$fit$w)>0){
      for(ind.fitw in 1:nrow(pc_list$fit$w)){ # go through weibull fitting parameter combis
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$w[ind.fitw,c("tte.dist", "prior.dist", "prior.belief")])
        # go through batches for one parcombi
        for(ind.batch in 1:pc_list$add$batch.nr){
          tryCatch({
            # try merge as new row to existing part
            merged.res.w = dplyr::bind_rows(merged.res.w,
                                 sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch))
            return(batch)
          },
          error=function(cond) {
            # if not, return a warning 
            warning(paste0(paste(c(pc_vect, "bADR_sim", ind.batch, ".RData") ,collapse="_"), " not found."))
          }
          )
          
        }
      }
    }
    
    if(nrow(pc_list$fit$dw)>0){
      for(ind.fitdw in 1:nrow(pc_list$fit$dw)){
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$dw[ind.fitdw,c("tte.dist", "prior.dist", "prior.belief")])
        # go through batches for one parcombi
        for(ind.batch in 1:pc_list$add$batch.nr){
          tryCatch({
            # try merge as new row to existing part
            merged.res.dw = dplyr::bind_rows(merged.res.dw,
                                 sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch))
          },
          error=function(cond) {
            # if not, return a warning 
            warning(paste0(paste(c(pc_vect, "bADR_sim", ind.batch, ".RData") ,collapse="_"), " not found."))
          }
          )
        }
      }
    }
    
    if(nrow(pc_list$fit$pgw)>0){
      for(ind.fitpgw in 1:nrow(pc_list$fit$pgw)){
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$pgw[ind.fitpgw,c("tte.dist", "prior.dist", "prior.belief")])
        # go through batches for one parcombi
        for(ind.batch in 1:pc_list$add$batch.nr){
          tryCatch({
            # try merge as new row to existing part
            merged.res.pgw = dplyr::bind_rows(merged.res.pgw,
                                 sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch))
          },
          error=function(cond) {
            # if not, return a warning 
            warning(paste0(paste(c(pc_vect, "bADR_sim", ind.batch, ".RData") ,collapse="_"), " not found."))
          }
          )
        }
      }
    }
  }
  
  # merge output for all tte.dists
  merged.res = dplyr::bind_rows(merged.res.w, merged.res.dw, merged.res.pgw)
  if(save == T){
    # save result
    path = pc_list$add$resultpath
    filename = "merged.res.RData"
    save(merged.res, file=paste0(path, "/", filename))
  }
  return(merged.res)
  
}
