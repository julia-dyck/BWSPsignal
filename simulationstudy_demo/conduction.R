#### simulationstudy_demo 
#### as R script

#### prep ----------------------------------------------------------------------
# packages

library(BWSPsignal) # signal detection test & simulation fcts.


####  prior elicitation --------------------------------------------------------
# try a few prior parameter combinations and see whether the resulting hazard 
# roughly matches the prior belief about the hazard form 

# Expected event time can also be taken into account for some guidance, but 
# should not be prioritized.
# The reason is that we do not expect the model to accurately fit the hazard of the 
# data, but only catch the rough form by distinguishing the cases
# constant vs decreasing vs unimodal vs increasing hazard.

# set prior means for Weibull parameters (ie powershape = 1 always):
365/4 * c(1,2,3) # expected event times under prior belief beginning, middle, end of study period
plot_pgw(scale = 1, shape = 1, powershape = 1)     # under prior belief "none"
plot_pgw(scale = 1, shape = 0.207, powershape = 1) # under prior belief "beginning"
plot_pgw(scale = 180, shape = 1, powershape = 1)   # under prior belief "middle" (cannot create a unimodal form)
plot_pgw(scale = 300, shape = 4, powershape = 1)   # under prior belief "end"

# set prior means for double Weibull setting:
# uncensored Weibull parameters are set equal to Weibull parameters
plot_pgw(scale = 1, shape = 1, powershape = 1)     # under prior belief "none"
plot_pgw(scale = 1, shape = 0.207, powershape = 1) # under prior belief "beginning"
plot_pgw(scale = 180, shape = 1, powershape = 1)   # under prior belief "middle" (cannot create a unimodal form)
plot_pgw(scale = 300, shape = 4, powershape = 1)   # under prior belief "end"
# for the censored Weibull parameters, imagine a priori expected hazard forms up to half of the study period
plot_pgw(scale = 1, shape = 1, powershape = 1)     # under prior belief "none"
plot_pgw(scale = 1, shape = 0.207, powershape = 1) # under prior belief "beginning"
plot_pgw(scale = 100, shape = 4, powershape = 1)   # under prior belief "middle" 
plot_pgw(scale = 1, shape = 1, powershape = 1)     # under prior belief "end"

# set prior means for Power generalized Weibull parameters:
plot_pgw(scale = 1, shape = 1, powershape = 1)     # under prior belief "none"
plot_pgw(scale = 20, shape = 5.5, powershape = 14) # under prior belief "beginning"
plot_pgw(scale = 180, shape = 1, powershape = 1)   # under prior belief "middle"
plot_pgw(scale = 300, shape = 4, powershape = 1)   # under prior belief "end"


#### specify parameter combinations for simulation study -----------------------

fp_list = sim.priors_template(prior.sds = 10) # setup prior template
# fill in prior template with values chosen in prior elicitation
fp_list$w[,2] = c(1, 1, 180, 300) # scale prior means
fp_list$w[,4] = c(1, 0.207, 1, 4) # shape prior means

fp_list$dw[,2] = c(1, 1, 180, 300) # scale prior means
fp_list$dw[,4] = c(1, 0.207, 1, 4) # shape prior means
fp_list$dw[,6] = c(1, 1, 100, 1)   # scale_c prior means
fp_list$dw[,8] = c(1, 0.207, 4, 1) # shape_c prior means

fp_list$pgw[,2] = c(1, 1, 20, 300) # scale prior means
fp_list$pgw[,4] = c(1, 0.207, 5.5, 4) # shape prior means
fp_list$pgw[,6] = c(1, 1, 14, 1) # powershape prior means

fp_list # filled fitpars.list

pc_list = sim.setup_sim_pars(N = c(500),
                             br = 0.1,
                             adr.rate = 0:1,
                             adr.relsd = 0.05,
                             study.period = 365,
                             
                             tte.dist = c("w", "dw", "pgw"),
                             prior.dist = c("fl", "ll", "fg", "gg"),
                             fitpars.list = fp_list,
                             
                             post.ci.type = c("ETI", "HDI"),
                             cred.level = seq(0.5,0.95, by = 0.05),
                             sensitivity.option = 1:3,
                             
                             reps = 10, # additional parameters
                             batch.size = 2,
                             
                             resultpath = paste0(getwd(), "/simulationstudy_demo"),
                             stanmod.chains = 4,
                             stanmod.iter = 11000,
                             stanmod.warmup = 1000
)


save(pc_list, file = paste0(getwd(), "/simulationstudy_demo/pc_list.RData"))


#### test for issues in prior choice -------------------------------------------

# test rope calculation given all prior distributions under none :
qlnorm(p = c(0.1,0.9), meanlog = logprior_repar(1, 10)[1], sdlog = logprior_repar(1, 10)[2])
qgamma(p = c(0.1,0.9), shape = gamprior_repar(1, 10)[1], rate = gamprior_repar(1, 10)[2])


# pilot modelling 
# to test for numerical issues in estimation given the chosen prior values under 
# all prior distributions:
dat = datagen_tte()




#### run simulation study ------------------------------------------------------

load(paste0(getwd(), "/simulationstudy_demo/pc_list.RData"))
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
load("aucs.RData")

# else, calculate AUC per scenario with
aucs = eval.calc_auc(pc_list)


## summarize AUC results (grouped averages) 

## stratified wrt. all prior distributional choices:
# ## average auc (over all scenarios)
# ## average auc | conditional on correct prior "when"-specification
file.edit("eval_auc_summary.R")

## after investigation of running time and effective sample size:
## focus on results with log-log-log as prior distributional choice
file.edit("eval_auc_summary_lll.R")



## END OF DOC
