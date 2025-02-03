#### evaluation of simstudy01 - masterfile ####

#### packages ------------------------------------------------------------------
library(dplyr)      # for data manipulation
library(tidyverse)  # for data manipulation
library(BWSPsignal) # signal detection test & simulation fcts.
library(ROCR)       # for AUC calculation
library(stringr)    # for table generation
library(xtable)     # for table generation

#### set working directory -----------------------------------------------------

# directory where the simulation study is stored [ADJUST TO YOUR LOCAL PATH]
setwd("C:/Users/jdyck/github_office_laptop/BWSPsignal/simulationstudy")


#### specify parameter combinations for simulation study -----------------------

# have a look at parameter combinations (pc) for simulations
load("pc_in_various_formats.RData")

# if you want to simulate new scenarios, have a look at
file.edit("setup_parameter_combination.R") 


#### run simulation study ------------------------------------------------------

file.edit("run_simulation.R") # to run all specified pc 100 times

file.edit("run_simulation_gap_filler.R") # when partly run, but not complete


#### merge results -------------------------------------------------------------

file.edit("merge_raw_results.R")


#### evaluation of non-convergance and estimation times ------------------------

## no of cancelled simulation runs for different prior distributional choices
file.edit("eval_cancelled_simulation_runs.R")

## running times for different dist assumptions -> boxplots --------------------
source("simulationstudy/eval_running_times_for_different_dist_assumptions.R")

## effective sample sizes for different dist assumptions -> boxplots -----------
source("simulationstudy/eval_investigate_effective_sample_sizes.R")

## auc results calculation------------------------------------------------------
#source("simulationstudy/eval_auc_calc.R")
# or
load("simulationstudy/eval_aucs.RData")


## summarize auc results (grouped averages etc.) -------------------------------

## for all distr. assumptions
## ## average auc
## ## average auc | conditional on correct prior "when"-specification
source("simulationstudy/eval_auc_summary.R")

## after investigation of running time and effective sample size:
## focus on results with log-log-log as distr. assumption
source("simulationstudy/eval_auc_summary_lll.R")



## END OF DOC
