#' Set up simulation parameters
#' 
#' With one partly interactive function
#' data generation, model fitting, number of simulation runs and evaluation are 
#' set up and gathered in a parameter combination (pc) list, from which the 
#' information are called in following steps of a simulation study.
#'
#' @param N A scalar or vector of sample sizes.
#' @param br A scalar or vector of background rates.
#' @param adr.rate A scalar or vector of adverse drug reaction rates.
#' @param adr.when A scalar or vector of expected event times (relative number, e.g. 0.5 matches half of study period).
#' @param adr.when.name A vector of description/short name of expected event times (ie. name of simulated truth)
#' @param adr.relsd A scalar or vector of relative standard deviations from the adverse drug reaction times.
#' @param study.period A scalar specifying the length of the study period.
#' ...
#' 
#' 
#' @details
#' The tuning parameters are used to evaluate the performance of different BWSP 
#' signal detection tests applied to data scenarios of interest.
#' The set of tuning parameters consisting of the choice of the
#' \enumerate{
#' \item distribution chosen for the tte model, must be a subset out of "w", "dw" 
#' and pgw",
#' \item prior belief about an adverse event being and adverse drug reaction, and if 
#' so, when it is expected to occur
#' (this will trigger promts asking for prior means and standard deviations for 
#' all parameters reflecting that belief; have a look at our vignette about 
#' prior elicitation to formalize the prior belief),
#' prior distribution chosen for scale and shape parameters, must be a subset 
#' out of "fgg", "ggg", "fll" and "lll",
#' \item type of posterior credible interval, must be a subset out of "ETI" 
#' (equal-tailed interval) and "HDI" (highest density interval).
#' 
#' 
#'
#' @export

sim.setup_sim_pars = function(N,                 # dgp parameters
                              br,                # |
                              adr.rate,          # |
                              adr.when,          # |
                              adr.when.label,     # |
                              adr.relsd,         # v
                              study.period,      # -
                              tte.dist,          # tuning parameters
                              prior.dist,        # |
                              post.ci.type,      # |
                              cred.level,        # v
                              sensitivity.option,# -
                              
                              reps = 100, # additional parameters
                              batch.size = 10,
                              batch.nr = reps/batch.size,
                              resultpath = paste0(getwd(), "/results_raw"),
                              stanmod.chains = 4,
                              stanmod.iter = 11000,
                              stanmod.warmup = 1000
){   
  
  adr.when.label = data.frame(adr.when.label = c("none", adr.when.label), adr.when = c(NA,adr.when))
  
  dgp_pars = sim.setup_dgp_pars(N = N,
                                br = br,
                                adr.when,
                                adr.rate = adr.rate,
                                adr.relsd = adr.relsd,
                                study.period = study.period)
  
  fit_pars = sim.setup_fit_pars(tte.dist = tte.dist,
                                prior.belief = c("none", adr.when.label),
                                prior.dist = prior.dist,
                                list.output = T)
  
  test_pars = sim.setup_test_pars(post.ci.type = post.ci.type,
                                  cred.level = cred.level,
                                  sensitivity.option = sensitivity.option)
  
  add_pars = list(reps = reps,
                  batch.size = batch.size,
                  batch.nr = reps/batch.size,
                  resultpath = resultpath,
                  stanmod.chains = stanmod.chains,
                  stanmod.iter = stanmod.iter,
                  stanmod.warmup = stanmod.warmup,
                  adr.when.label = adr.when.label
  )
  
  input_args = list(N = N, 
                    br = br, 
                    adr.rate = adr.rate, 
                    adr.when = adr.when,
                    adr.when.label = adr.when.label,
                    adr.relsd = adr.relsd, 
                    study.period = study.period,
                    tte.dist = tte.dist, 
                    prior.belief = c("none", adr.when.label), 
                    prior.dist = prior.dist, 
                    post.ci.type = post.ci.type, 
                    cred.level = cred.level, 
                    sensitivity.option = sensitivity.option,
                    reps = reps, 
                    batch.size = batch.size, 
                    resultpath = resultpath, 
                    stanmod.chains = stanmod.chains, 
                    stanmod.iter = stanmod.iter, 
                    stanmod.warmup = stanmod.warmup
  )
  
  cat(paste0("Each combination of sample scenario and prior specification leads to a total of ",
             nrow(dgp_pars) * sum(nrow(fit_pars$w), nrow(fit_pars$dw), nrow(fit_pars$pgw)),
             " different simulation settings. ",
             "Each simulation scenario’s data generation and posterior estimation will be repeated ", 
             reps,
             " times. "))
  
  pc_table = list()  # parameters in table format s.t. one row = one simulation scenario
  pc_table$w = dplyr::cross_join(dgp_pars, fit_pars$w[c("tte.dist", "prior.dist", "prior.belief")])
  pc_table$dw = dplyr::cross_join(dgp_pars, fit_pars$dw[c("tte.dist", "prior.dist", "prior.belief")])
  pc_table$dw = dplyr::cross_join(dgp_pars, fit_pars$dw[c("tte.dist", "prior.dist", "prior.belief")])
  
  pc_table = dplyr::bind_rows(pc_table)
  
  
  sim_pars = list(dgp = dgp_pars, fit = fit_pars, test = test_pars, add = add_pars, input = input_args, pc_table = pc_table)
  
  return(sim_pars)
}

pc_list_testsetup = sim.setup_sim_pars(N = 500,
                                       br = 0.1,
                                       adr.rate = 0:1,
                                       adr.when = 0.25,
                                       adr.when.label = "beginning",
                                       adr.relsd = 0.05,
                                       study.period = 365,
                                       
                                       tte.dist = c("w", "dw"),
                                       prior.dist = c("fg", "ll"),
                                       
                                       post.ci.type = c("ETI", "HDI"),
                                       cred.level = seq(0.5,0.95, by = 0.05),
                                       sensitivity.option = 1:3,
                                       
                                       reps = 6, # additional parameters
                                       batch.size = 2,
                                       batch.nr = reps/batch.size,
                                       resultpath = paste0(getwd(), "/results_test"),
                                       stanmod.chains = 4,
                                       stanmod.iter = 11000,
                                       stanmod.warmup = 1000
)


## END OF DOC
