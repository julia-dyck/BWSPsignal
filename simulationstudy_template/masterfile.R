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
setwd("simulationstudy_template")


#### specify parameter combinations for simulation study -----------------------

# if you want to simulate new scenarios, have a look at
file.edit("setup_parameters.R") 


#### run simulation study ------------------------------------------------------

load("pc_list.RData")
sim.run(pc_list = pc_list)


#### merge results -------------------------------------------------------------

load("pc_list.RData")
# merge result batches and save
sim.merge_results(pc_list, save = T)
# load merged results into environment
load(paste0(pc_list$add$resultpath, "/res.RData"))


#### convergence issues, execution times and effective sample sizes ------------
   # for different prior distributional choices

## no of simulation runs with convergence issues
eval.non_conv_cases(pc_list)

## execution times 
eval.execution_times(pc_list)

## effective sample sizes ## HIER WEITER
eval.eff_sample_sizes(pc_list, threshold = 10000)


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
