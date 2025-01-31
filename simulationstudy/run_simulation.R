#### simulation 01  

# preparation __________________________________________________________________

#### set working directory -----------------------------------------------------
setwd("C:/Users/jdyck/github_office_laptop/BWSPsignal") # set to the directory where the simulation study is stored

setwd("simulationstudy")

#### packages ------------------------------------------------------------------
library(dplyr)
library(tidyverse)
#install.packages("rstan", version = "2.26.17",
#                 repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(rstan)
parallel::detectCores() # how many cores are available?
options(mc.cores = 4)
rstan_options(auto_write = TRUE)

library(BWSPsignal)

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
