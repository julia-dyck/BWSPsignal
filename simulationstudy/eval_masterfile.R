#### evaluation of simstudy01 - masterfile ####

#### packages ------------------------------------------------------------------
library(dplyr)      # for data manipulation
library(tidyverse)  # for data manipulation
library(bADRfcts)   # signal detection tool components
library(ROCR)       # for AUC calculation
library(stringr)    # for table generation
library(xtable)     # for table generation


## evaluation prep (packages, setwd, loadf parcombi tables) --------------------
file.edit("simulationstudy/eval_prep.R")

## number of cancelled simulation runs for different dist assumptions
source("simulationstudy/eval_cancelled_simulation_runs.R")

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
