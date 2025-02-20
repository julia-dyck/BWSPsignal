#### simulation 01  

# preparation __________________________________________________________________

#### loading of parameter combination table ------------------------------------
load("pc_in_various_formats.RData")

### result path
resultpath = paste0(getwd(), "/results_raw")


# conduction of simulations ____________________________________________________

for(pcind in 1:nrow(pc)){
  for(i in 1:10){
    sim.repeat.1.scenario(batch.nr=i, reps = 2, pc = pc[pcind,],
                          save = T, path = resultpath)
  }
}



## END OF DOC
