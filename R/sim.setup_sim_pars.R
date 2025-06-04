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
#' @param adr.relsd A scalar or vector of relative standard deviations from the adverse drug reaction times.
#' @param study.period A scalar specifying the length of the study period.
#' @param tte.dist A character string indicating the modelling approach. Options are
#' \code{"w", "dw", "pgw"} (see also \link{bwsp_model}).
#' @param prior.dist A character string indicating the prior distribution for the
#' parameters of the pgW distribution. Options are 
#' \code{"fg", "fl", "gg", "ll"} (see also \link{bwsp_model}).
#' @param post.ci.type A character string indicating whether to extract equal tailed
#' intervals (\code{"ETI"}) or highest posterior density intervals (\code{HDI}) as
#' credibilty interval/region for BWSP testing (see \link{bwsp_test}).
#' @param cred.level A scalar or vector of credibility levels to be tried for construction
#' of region of practical equivalence (ROPE) and posterior credibility interval (CI).
#' @param sensitivity.option A scalar or vector of sensitivity options to be tried for the BWSP test.
#' @param reps The number of repetitions for each simulation scenario.
#' @param batch.size The number of simulation repetitions to be saved in a batch
#' @param batch.nr per default \code{reps/batch.size}; the number of batch files 
#' in resultpath.
#' @param resultpath The directory where intermediate results of the simulation
#' are saved.
#' @param stanmod.chains The number of Markov chains to run.
#' @param stanmod.iter The total number of iterations per chain (including warmup).
#' @param standmod.warmup The number of warmup iterations per chain.
#' 
#' 
#' @details
#' The tuning parameters are used to evaluate the performance of different BWSP 
#' signal detection tests applied to data scenarios of interest.
#' The set of tuning parameters consisting of the choice of the
#' \enumerate{
#' \item distribution chosen for the tte model, must be a subset out of "w", "dw" 
#' and pgw",
#' \item specification of prior means and standard deviations to reflect the 
#' prior belief about an expected event time toward the beginning/1st quarter,
#' middle/2nd quarter or end/3rd quarter of the study period,
#' prior distribution chosen for scale and shape parameters, must be a subset 
#' out of "fgg", "ggg", "fll" and "lll",
#' \item type of posterior credible interval, must be a subset out of "ETI" 
#' (equal-tailed interval) and "HDI" (highest density interval).
#' }
#' 
#' Batch saving is done to prevent losing simulation results in case of an
#' interuption of simulation.
#'
#' @export

sim.setup_sim_pars = function(N,                 # dgp parameters
                              br,                # |
                              adr.rate,          # |
                              adr.relsd,         # v
                              study.period,      # -
                              tte.dist,          # tuning parameters
                              prior.dist,        # |
                              fitpars.list = NULL, # |
                              post.ci.type,      # |
                              cred.level,        # v
                              sensitivity.option,# -
                              
                              reps = 100,        # additional parameters
                              batch.size = 10,
                              batch.nr = reps/batch.size,
                              resultpath = paste0(getwd(), "/results_raw"),
                              stanmod.chains = 4,
                              stanmod.iter = 11000,
                              stanmod.warmup = 1000
){   
  
  dgp_pars = sim.setup_dgp_pars(N = N,
                                br = br,
                                adr.when = c(0, 0.25, 0.5, 0.75), # fixed (reduce complexity)
                                adr.rate = adr.rate,
                                adr.relsd = adr.relsd,
                                study.period = study.period)
  
  
  
  
  fit_pars = sim.setup_fit_pars(tte.dist = tte.dist,
                                prior.belief = c("none", "beginning", "middle", "end"), # fixed (matching adr.when)
                                prior.dist = prior.dist,
                                fit_pars_list = fitpars.list)
  
  test_pars = sim.setup_test_pars(post.ci.type = post.ci.type,
                                  cred.level = cred.level,
                                  sensitivity.option = sensitivity.option)
  
  add_pars = list(reps = reps,
                  batch.size = batch.size,
                  batch.nr = reps/batch.size,
                  resultpath = resultpath,
                  stanmod.chains = stanmod.chains,
                  stanmod.iter = stanmod.iter,
                  stanmod.warmup = stanmod.warmup
  )
  
  input_args = list(N = N, 
                    br = br, 
                    adr.rate = adr.rate, 
                    adr.when = c(0, 0.25, 0.5, 0.75), # fixed (reduce complexity)
                    adr.relsd = adr.relsd, 
                    study.period = study.period,
                    tte.dist = tte.dist, 
                    prior.belief = c("none", "beginning", "middle", "end"), # fixed (matching adr.when)
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
  if(!is.null(fit_pars$w)){
    pc_table$w = dplyr::cross_join(dgp_pars, fit_pars$w[c("tte.dist", "prior.dist", "prior.belief")])
  }
  if(!is.null(fit_pars$dw)){
    pc_table$dw = dplyr::cross_join(dgp_pars, fit_pars$dw[c("tte.dist", "prior.dist", "prior.belief")])
  }
  if(!is.null(fit_pars$pgw)){
    pc_table$pgw = dplyr::cross_join(dgp_pars, fit_pars$pgw[c("tte.dist", "prior.dist", "prior.belief")])
  }
  pc_table = dplyr::bind_rows(pc_table)
  
  
  sim_pars = list(dgp = dgp_pars, fit = fit_pars, test = test_pars, add = add_pars, input = input_args, pc_table = pc_table)
  
  return(sim_pars)
}


## END OF DOC
