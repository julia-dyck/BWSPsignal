### monitor progress of simulation study

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


### monitor progress ___________________________________________________________

# how many simulation batches of which pc are already run?
# (each batch contains 10 simulation runs of the specified scenario)
progress = sim.monitor.progress(pc_table = pc.numeric, 
                                wd = resultpath, 
                                batch_max = 10)

# save(progress, file = getwd()) # if to be saved for later