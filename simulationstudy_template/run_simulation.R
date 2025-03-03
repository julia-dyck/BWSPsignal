#### simulation 01  

# preparation __________________________________________________________________

#### loading of parameter combination table ------------------------------------
load("pc_in_various_formats.RData")

### result path
resultpath = paste0(getwd(), "/results_raw")


# conduction of simulations ____________________________________________________

# fct with huge pc list as input sim.run(pc.list = pc)



for(dgpind in 1:nrow(pc_list$dgp)){
  for(batchind in 1:nr.batches){
    sim.repeat.1.scenario(batch.ind = batchind, reps.per.batch = 10, dgp = pc_list$dgp[dgpind,],
                          save = T, path = resultpath)
  }
}



## END OF DOC

## new, more flexible version only depending on huge pc_list object




for(ind.dgp in 1:nrow(pc_list$dgp)){      # go through dgp scenarios (per row)
  if(nrow(pc_list$fit$w)>0){
    for(ind.fitw in 1:nrow(pc_list$fit$w)){ # go through weibull fitting parameter combis
      # set up one dgp+fit combination
      pc = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$w[ind.fitw,c("tte.dist", "prior.dist", "prior.belief")])
      # repeat modelling for one scenario
      for(ind.batch in 1:nr.batches){
        sim.repeat.1.scenario(batch.ind = ind.batch,
                              reps.per.batch = 10,
                              pc = pc,
                              save = T,
                              path = resultpath)
      }
    }
  }
  
  if(nrow(pc_list$fit$dw)>0){
    for(ind.fitdw in 1:nrow(pc_list$fit$dw)){
      # set up one dgp+fit combination
      pc = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$dw[ind.fitdw,c("tte.dist", "prior.dist", "prior.belief")])
      #
      
    }
  }
  
  if(nrow(pc_list$fit$pgw)>0){
    for(ind.fitpgw in 1:nrow(pc_list$fit$pgw)){
      # set up one dgp+fit combination
      pc = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$pgw[ind.fitpgw,c("tte.dist", "prior.dist", "prior.belief")])
      print(pc)
    }
  }
  
}
