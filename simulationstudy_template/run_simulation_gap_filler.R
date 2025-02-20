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

### check progress of simulation runs __________________________________________

### result path ----------------------------------------------------------------
resultpath = paste0(getwd(), "/results_raw")


# extract indices of not yet run simulations -----------------------------------

file.edit("run_monitor_progress.R") # run it to get progress as vector

load("progress.RData") # run if progress already 
                       # monitored and save in wd


# conduction of missing simulations ____________________________________________

# parameter combination indices with 0 batches run -----------------------------
ind_0_runs = which(progress == 0)
progress[ind_0_runs]
pc[ind_0_runs,]

for(pcind in ind_0_runs){ # run 10 batches of 10 reps each
  for(i in 1:10){
    sim.repeat.1.scenario(batch.nr=i, reps = 10, pc = pc[pcind,],
                          save = T, path = getwd())
  }
}

# parameter combi indices with 1 batch run
ind_1_runs = which(progress == 1)
progress[ind_1_runs]
pc[ind_1_runs,]

for(pcind in ind_1_runs){ # run 9 batches of 10 reps each
  for(i in 2:10){ 
    sim.repeat.1.scenario(batch.nr=i, reps = 10, pc = pc[pcind,],
                          save = T, path = getwd())
  }
}

# parameter combi indices with ... batches run
# -> copy and adjust



## END OF DOC
