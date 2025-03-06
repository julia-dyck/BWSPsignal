#### simulation 01  

# preparation __________________________________________________________________

#### loading of parameter combination table ------------------------------------
load("pc_list.RData")

# conduction of simulations ____________________________________________________


## new, more flexible version only depending on huge pc_list object
## embed that in a fct? (maybe like sim.run(pc_list, fill = F))

s = 0

for(ind.dgp in 1:nrow(pc_list$dgp)){      # go through dgp scenarios (per row)
  if(nrow(pc_list$fit$w)>0){
    for(ind.fitw in 1:nrow(pc_list$fit$w)){ # go through weibull fitting parameter combis
      # set up one dgp+fit combination
      pc = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$w[ind.fitw,c("tte.dist", "prior.dist", "prior.belief")])
      # repeat modelling for one scenario
      for(ind.batch in 1:pc_list$add$batch.nr){
        # sim.repeat.1.scenario(batch.ind = ind.batch,
        #                       reps.per.batch = 10,
        #                       pc = pc,
        #                       save = T,
        #                       path = resultpath)
        print(paste(c(pc, "bADR_sim", ind.batch, ".RData"), collapse="_"))
        print(s); s = s+ 1
      }
    }
  }
  
  if(nrow(pc_list$fit$dw)>0){
    for(ind.fitdw in 1:nrow(pc_list$fit$dw)){
      # set up one dgp+fit combination
      pc = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$dw[ind.fitdw,c("tte.dist", "prior.dist", "prior.belief")])
      # repeat modelling for one scenario
      for(ind.batch in 1:pc_list$add$batch.nr){
        # sim.repeat.1.scenario(batch.ind = ind.batch,
        #                       reps.per.batch = 10,
        #                       pc = pc,
        #                       save = T,
        #                       path = resultpath)
        print(paste(c(pc, "bADR_sim", ind.batch, ".RData"), collapse="_"))
        print(s); s = s+ 1
      }
    }
  }
  
  if(nrow(pc_list$fit$pgw)>0){
    for(ind.fitpgw in 1:nrow(pc_list$fit$pgw)){
      # set up one dgp+fit combination
      pc = sim.gather_pc_vect(pc_list$dgp[ind.dgp,], pc_list$fit$pgw[ind.fitpgw,c("tte.dist", "prior.dist", "prior.belief")])
      # repeat modelling for one scenario
      for(ind.batch in 1:pc_list$add$batch.nr){
        # sim.repeat.1.scenario(batch.ind = ind.batch,
        #                       reps.per.batch = 10,
        #                       pc = pc,
        #                       save = T,
        #                       path = resultpath)
        print(paste(c("bADR_sim", pc, ind.batch, ".RData"), collapse="_"))
        print(s); s = s+ 1
      }
    }
  }
  
}



## END OF DOC