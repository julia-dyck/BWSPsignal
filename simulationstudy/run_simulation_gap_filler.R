#### run simulation 01 - gap filler
#### parameter combination pc[ind_0_runs 1:30]

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


# extract indices of not yet run simulations ___________________________________

load("progress_YYYY_MM_DD.RData")


### parameter combinations to be run -------------------------------------------
load("pc_fr_complete.RData")
load("progress_2023_05_24.RData")



# conduction of simulations ____________________________________________________

ind_0_runs = which(progress == 0)
progress[ind_0_runs]
pc[ind_0_runs,]

for(pcind in ind_0_runs){
  for(i in 1:10){
    sim.repeat.1.scenario(batch.nr=i, reps = 10, pc = pc[pcind,],
                          save = T, path = getwd())
  }
}


ind_1_runs = which(progress == 1)
progress[ind_1_runs]
pc[ind_1_runs,]

for(pcind in ind_1_runs){
  for(i in 1:10){
    sim.repeat.1.scenario(batch.nr=i, reps = 10, pc = pc[pcind,],
                          save = T, path = getwd())
  }
}

## END OF DOC
