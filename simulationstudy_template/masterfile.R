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
setwd("C:/Users/jdyck/github_office_laptop/BWSPsignal/simulationstudy_template")


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


#### convergence issues, execution times and effective sample sizes ------------
   # for different prior distributional choices

## no of simulation runs with convergence issues
file.edit("eval_cancelled_simulation_runs.R")

## execution times 
file.edit("eval_execution_times.R")

## effective sample sizes
file.edit("eval_effective_sample_sizes.R")


#### AUC performance -----------------------------------------------------------
   # (AUC: area under the ROC curve)

# if already calculated and saved, load AUC results per scenario with
load("eval_aucs.RData")

# else, calculate AUC per scenario with
file.edit("eval_auc_calc.R")


## summarize AUC results (grouped averages) 

## stratified wrt. all prior distributional choices:
# ## average auc (over all scenarios)
# ## average auc | conditional on correct prior "when"-specification
file.edit("eval_auc_summary.R")

## after investigation of running time and effective sample size:
## focus on results with log-log-log as prior distributional choice
file.edit("eval_auc_summary_lll.R")



## END OF DOC
