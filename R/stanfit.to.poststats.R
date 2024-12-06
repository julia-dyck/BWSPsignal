#' extract posterior summary statistics and credibility intervals
#'
#' Given a stanfit object, the function extracts statistics about the posterior
#' distribution of the shape parameters nu and gamma.
#' These shall give an overview and provide the basis for the adr signal detection
#' testing in one vector.
#'
#' @param stanfit.object the estimated stan model output
#' @param cred.niveaus
#'
#' @return Information about the posterior distributions stored in a vector
#'         consisting of the following entry parts first for parameter nu followed by the same statistics regarding
#'         parameter gamma:
#'
#' \item{nu/ga.post.stats}{Summary statistics from the stanfit object about the posterior distribution, namely \code{mean, se_mean, sd},
#'                  and bayesian convergence diagnostic measures, namely \cod{n_eff, Rhat}}
#' \item{nu/ga.eti}{equitailed posterior (credibility) intervals specified by their lower and upper boundaries;
#'                  one interval is derived for each specified credibility niveau}
#' \item{nu/ga.hdi}{highest posterior density intervals specified by their lower and upper boundaries;
#'                  one interval is derived for each specified credibility niveau}
#' \item{nu/ga.per}{percentiles for the posterior density in order to estimate the incorporated probability mass within a
#'                  specified region of practical equivalence (rope) for the null value of the statistical test that will be
#'                  conducted after the fitting procedure}
#'
#'  @details The storing in one long vector for each parameter is motivated by the
#'  goal to conduct a simulation study, where all relevant statistics per run ars stored columnwise
#'  and the repeated runs for one data scenario are store rowwise.
#'
#' @export




# function --------------------------------------------------------------------
stanfit.to.poststats = function(stanfit.object, cred.niveaus = seq(0.5, 0.95, by = 0.05)){
  require(rstan)
  obj = stanfit.object

  ######## extraction of posterior summary stats

  # add in: "if the scale parameter is fittet: ...
  if( ncol(as.matrix(stanfit.object)) == 4 ){
    post_summary = rstan::summary(obj, pars = c("theta", "nu", "gamma"), probs = c())$summary
    poststats = list(th = post_summary[1,], nu = post_summary[2,], ga = post_summary[3,])
  }
  # else (ie. if the scale parameter is fix):
  if( ncol(as.matrix(stanfit.object)) == 3 ){
  post_summary = rstan::summary(obj, pars = c("nu", "gamma"), probs = c())$summary
  poststats = list(th = rep(NA, 5), nu = post_summary[1,], ga = post_summary[2,])
  }

  th.post.stats = poststats$th
  names(th.post.stats) = paste0("th.po.", names(poststats$nu))

  nu.post.stats = poststats$nu
  names(nu.post.stats) = paste0("nu.po.", names(nu.post.stats))

  ga.post.stats = poststats$ga
  names(ga.post.stats) = paste0("ga.po.", names(ga.post.stats))


  ######## extraction of posterior samples for credible interval calculation
  post.sample = rstan::extract(obj, pars = c("nu", "gamma"))
  # results in
  # ## post.sample$nu = posterior sample of nu, representing the posterior distribution
  # ## post.sample$gam = posterior sample of gamma, representing the posterior distribution

  # credibility/confidence niveaus 1-alpha
    # 5% ETI and HDI
    # 10% ETI and HDI
    # ...
    # 95% ETI and HDI

  # calculate equal tailed intervals (ETIs)
  nu.eti = matrix(nrow = length(cred.niveaus), ncol = 2)
  for(i in 1:length(cred.niveaus)){
    nu.eti[i,] = quantile(post.sample$nu, probs = 0.5 + c(-1,1)*cred.niveaus[i]/2)
  }
  nu.eti = c(t(nu.eti)) # transform to vector
  names(nu.eti) = paste0("nu.eti",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )


  ga.eti = matrix(nrow = length(cred.niveaus), ncol = 2)
  for(i in 1:length(cred.niveaus)){
    ga.eti[i,] = quantile(post.sample$gamma, probs = 0.5 + c(-1,1)*cred.niveaus[i]/2)
  }
  ga.eti = c(t(ga.eti)) # transform to vector
  names(ga.eti) = paste0("ga.eti",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )


  # calculate highest density intervals (HDIs)

  nu.hdi = t(sapply(cred.niveaus, HDInterval::hdi, object = post.sample$nu))
  nu.hdi = c(t(nu.hdi)) # transform to vector
  names(nu.hdi) = paste0("nu.hdi",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )


  ga.hdi = t(sapply(cred.niveaus, HDInterval::hdi, object = post.sample$gamma))
  ga.hdi = c(t(ga.hdi)) # transform to vector
  names(ga.hdi) = paste0("ga.hdi",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )

  # calculate percentiles

  nu.per = quantile(post.sample$nu, probs = (0:100)/100)
  names(nu.per) = paste0("nu.per", names(nu.per))

  ga.per = quantile(post.sample$gamma, probs = (0:100)/100)
  names(ga.per) = paste0("ga.per", names(ga.per))

 ret.vect = t(as.data.frame(c(th.post.stats,
                              nu.post.stats, nu.eti, nu.hdi, nu.per,
                              ga.post.stats, ga.eti, ga.hdi, ga.per)))
 rownames(ret.vect) = NULL
 return(ret.vect)
}




# function testin --------------------------------------------------------------

# stanfit.to.poststats(stanfit.object = testout[[1]], cred.niveaus = seq(0.5, 0.95, by = 0.05))
# stats.out = stanfit.to.poststats(testout[[2]])
# stats.out
# dim(stats.out)
# View(stats.out)

## END of Doc
