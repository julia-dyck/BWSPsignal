#' Set up simulation parameters
#' 
#' Sets up parameters for a simulation study to tune the Bayesian Weibull shape parameter (BWSP) test.
#' Simulation parameters encompass data generating process (DGP) parameters (\code{N} to \code{study.period}),
#' tuning parameters for the BWSP test (\code{tte.dist} to \code{sensitivity.option}), 
#' and additional parameters (\code{reps} to \code{stanmod.warmup}).
#'
#' @param N vector of sample sizes
#' @param br vector of background rates (observed in population on average)
#' @param adr.rate vector of adverse drug reaction rates as proportions of the background rates
#' @param adr.relsd scalar or vector of relative standard deviations from the adverse drug reaction times
#' @param study.period scalar specifying the length of the study period
#' @param tte.dist character vector specifying one or multiple modelling approaches; options are
#' \code{"w", "dw", "pgw"} (see also \code{\link{bwsp_model}})
#' @param prior.dist character indicating the prior distribution for the parameters 
#' of the tte distribution; options are "fg", "fl", "gg", "ll" (see details)
#' @param fitpars.list list with one data.frame per tte distribution containing
#' the prior specifications for model fitting; setup with \code{\link{sim.priors_template}}
#' @param post.ci.type character indicating whether to extract equal tailed
#' intervals (\code{"ETI"}) or highest posterior density intervals (\code{"HDI"}) as
#' credibilty interval (CI) for BWSP testing (see \code{\link{bwsp_test}})
#' @param cred.level vector of credibility levels used for construction
#' of region of practical equivalence (ROPE) and posterior CI
#' @param sensitivity.option vector of sensitivity options for the BWSP test (see \code{\link{bwsp_test}})
#' @param reps number of repetitions for each simulation scenario
#' @param batch.size number of simulation repetitions to be saved in a batch (see details)
#' @param batch.nr per default \code{reps/batch.size}; number of batch files
#' @param resultpath directory where intermediate results of the simulation
#' are saved
#' @param stanmod.chains number of Markov chains (see \code\link[rstan]{sampling}})
#' @param stanmod.iter total number of iterations per chain including warmup (see \code\link[rstan]{sampling}})
#' @param stanmod.warmup number of warmup iterations per chain (see \code\link[rstan]{sampling}})
#' 
#' 
#' @details
#' The purpose of the simulation study is to evaluate the performance of different BWSP 
#' signal detection test tunings for data scenarios of interest following the tuning
#' scheme developed in \insertCite{dyck2024bpgwsppreprint;textual}{BWSPsignal}.
#' 
#' DGP parameters (\code{N} to \code{study.period}) should 
#' reflect the data characteristics of interest. Given the intention to apply the
#' WSP test to a specific real data set, the DGP parameters should reflect its features.
#' Within the simulation, data is generated with \code{\link{sim.datagen_tte}}.
#' 
#' Tuning parameters for the BWSP test (\code{tte.dist} to \code{sensitivity.option})
#' lead to a range of tuning combinations evaluated during the simulation study to
#' find the best test tuning. 
#' 
#' Argument \code{fitpars.list} contains the prior means and sds for the
#' prior distributions (\code{prior.dist}) for all scale and shape parameters.
#' A template for the \code{fitpars.list} to be filled can be generated with 
#' \code{\link{sim.priors_template}}.
#' 
#' 
#'  
#' and additional parameters (\code{reps} to \code{stanmod.warmup}).
#' 
#' Batch saving is done to prevent losing simulation results in case of an
#' interruption of simulation.
#' 
#' @references 
#' \insertAllCited{}
#' 
#' 
#' @examples
#'
#' @export

sim.setup_sim_pars = function(N,                 # dgp parameters
                              br,                # |
                              adr.rate,          # |
                              adr.relsd,         # v
                              study.period,      # -
                              tte.dist,          # tuning parameters
                              prior.dist,        # |
                              fitpars.list,      # |
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
  if(nrow(fit_pars$w) > 0){
    pc_table$w = dplyr::cross_join(dgp_pars, fit_pars$w[c("tte.dist", "prior.dist", "prior.belief")])
  }
  else{pc_table$w = NULL}
  if(nrow(fit_pars$dw) > 0){
    pc_table$dw = dplyr::cross_join(dgp_pars, fit_pars$dw[c("tte.dist", "prior.dist", "prior.belief")])
  }
  else{pc_table$dw = NULL}
  if(nrow(fit_pars$pgw) > 0){
    pc_table$pgw = dplyr::cross_join(dgp_pars, fit_pars$pgw[c("tte.dist", "prior.dist", "prior.belief")])
  }
  else{pc_table$pgw = NULL}
  pc_table = dplyr::bind_rows(pc_table)
  
  
  sim_pars = list(dgp = dgp_pars, fit = fit_pars, test = test_pars, add = add_pars, input = input_args, pc_table = pc_table)
  
  return(sim_pars)
}


## END OF DOC
