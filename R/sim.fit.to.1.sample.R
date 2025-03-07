#' Simstudy: apply all models to one specific sample
#'
#' Conducts all steps from data generation to simulation output generation including:
#' 
#' - data generation based on provided parameter combination
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
#' @param pc parameter combination 
#'
#' @return a data frame with 16 rows. Each row contains statistics for one of the 
#' 4x4 prior (fl, ll, fg, gg) and model (no ADR expected, ADR expected around 
#' 1st, 2nd, 3rd quartile of observation period) alternatives.
#'
#'
#' @export
#'
#'


sim.fit.to.1.sample = function(pc, pc_list){

  ### Data simulation
  ttedat = datagen_tte(genpar = c(pc$N, pc$br, pc$adr.rate, pc$adr.when, pc$adr.relsd, pc$study.period))

  ### tte and prior data preparation
  datstan = sim.fit.prep(ttedat = ttedat, pc = pc, pc_list = pc_list)

  ### Model fitting
    mod = fit_mod_tte(datstan = datstan,
                      tte.dist = pc$tte.dist,
                      prior.dist = pc$prior.dist)

  ### extracting relevant statistics #TO BE ADJUSTED YET
  stats = cbind(pc,
                sim.stanfit.to.fitstats(stanfit.object = mod,
                                    stan.dat = datstan),
                as.data.frame(sim.stanfit.to.poststats(stanfit.object = mod,
                                                       cred.niveaus = cred.levels
                                                         )))

  return(stats)

}



## END OF DOC