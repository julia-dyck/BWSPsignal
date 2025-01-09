#' Simstudy: apply all models to one specific sample
#'
#' Conducts all steps from data input to simulation output generation including:
#'
#' - data preparation for stanmodel for all prior assumptions
#'
#' - fitting all model alternatives
#'
#' - extracting and returning relevant statistics from stan output:
#'   - general information "info" (noch meta), eg. prior mean and sds, fitting specifications etc.
#'   - information about the posterior distributions of nu and gamma "post"
#'   - credibility intervals for the tests (will be conducted afterwards)
#'   - 0 to 100 percentiles of the posterior distribution (to roughly estimate the
#'     probability mass within the region of practical equivalence for the null value)
#'
#' @param survdat time-event-sample
#'
#' @return a data frame with 16 rows. Each row contains statistics for one of the 4x4 prior and model alternatives.
#'
#'
#' @export
#'
#'


sim.fit.to.1.sample = function(pc, cores = 1){

  ### Data simulation
  survdat = datagen_tte(genpar = pc)

  ### Data and prior prep
  datstan = sim.fit.prep(survdat = survdat, pc = pc)

  dist.ass = pc[7]
  adr.ass = pc[8]

  ### Model fitting
  if(dist.ass == "fix.gam.gam"){
    mod = fit.fgg(datstan = datstan, cores = cores)
    mod@model_name = "fix.gam.gam" # manually, because not working automatically
  }
  if(dist.ass == "gam.gam.gam"){
    mod = fit.ggg(datstan = datstan, cores = cores)
    mod@model_name = "gam.gam.gam" # manually, because not working automatically
  }
  if(dist.ass == "fix.log.log"){
    mod = fit.fll(datstan = datstan, cores = cores)
    mod@model_name = "fix.log.log" # manually, because not working automatically
  }
  if(dist.ass == "log.log.log"){
    mod = fit.lll(datstan = datstan, cores = cores)
    mod@model_name = "log.log.log" # manually, because not working automatically
  }

  ### extracting relevant statistics
  stats = cbind(pc,
                stanfit.to.fitstats(stanfit.object = mod,
                                    stan.dat = datstan),
                as.data.frame(stanfit.to.poststats(stanfit.object = mod,
                                                   cred.niveaus = seq(0.5, 0.95, by = 0.05))))

  return(stats)

}


#  testing ---------------------------------------------------------------------

# library(rstan)
# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)
#
# testft1s = sim.fit.to.1.sample(dat = testdatlist) # TESTEN
# testft1s



## END OF DOC


