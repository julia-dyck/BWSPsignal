#### simulation 01  

# preparation __________________________________________________________________

#### loading of parameter combination table ------------------------------------
load("pc_list.RData")

# conduction of simulations ____________________________________________________


## new, more flexible version only depending on huge pc_list object
## embed that in a fct? (maybe like sim.run(pc_list, fill = F))


    for(ind.dgp in 1:nrow(pc_list$dgp)){      # go through dgp scenarios (per row)
    if(nrow(pc_list$fit$w)>0){
      for(ind.fitw in 1:nrow(pc_list$fit$w)){ # go through weibull fitting parameter combis
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$w[ind.fitw,c("tte.dist", "prior.dist", "prior.belief")])
        
        for(ind.batch in 1:pc_list$add$batch.nr){
          
          tryCatch({
              # check whether batch file already exists
              sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch)
              message("File exists.")
            },
            error=function(cond) {
              # if not, generate result batch file
              # repeat modelling for one scenario
              sim.repeat.1.scenario(pc = pc_vect,
                                    pc_list = pc_list,
                                    batch.ind = ind.batch
                                    )
            }
          )
          
        }
      }
    }
    
    if(nrow(pc_list$fit$dw)>0){
      for(ind.fitdw in 1:nrow(pc_list$fit$dw)){
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$dw[ind.fitdw,c("tte.dist", "prior.dist", "prior.belief")])
        # repeat modelling for one scenario
        for(ind.batch in 1:pc_list$add$batch.nr){
          tryCatch({
            # check whether batch file already exists
            sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch)
            message("File exists.")
          },
          error=function(cond) {
            # if not, generate result batch file
            sim.repeat.1.scenario(pc = pc_vect,
                                  pc_list = pc_list,
                                  batch.ind = ind.batch
            )
          }
          )
        }
      }
    }
    
    if(nrow(pc_list$fit$pgw)>0){
      for(ind.fitpgw in 1:nrow(pc_list$fit$pgw)){
        # set up one dgp+fit combination
        pc_vect = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$pgw[ind.fitpgw,c("tte.dist", "prior.dist", "prior.belief")])
        # repeat modelling for one scenario
        for(ind.batch in 1:pc_list$add$batch.nr){
          tryCatch({
            # check whether batch file already exists
            sim.load.scenario(pc = pc_vect, wd= pc_list$add$resultpath, batchnr = ind.batch)
            message("File exists.")
          },
          error=function(cond) {
            # if not, generate result batch file
            sim.repeat.1.scenario(pc = pc_vect,
                                  pc_list = pc_list,
                                  batch.ind = ind.batch
            )
          }
          )
        }
      }
    }
  }

## END OF DOC