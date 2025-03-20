#' extract posterior summary statistics and credibility intervals
#'
#' Given a stanfit object returned by \code{\link{fit_pgw_tte}}, the function extracts statistics about the posterior
#' distribution of the shape parameters nu and gamma.
#' These shall give an overview and provide the basis for the adr signal detection
#' testing in one vector.
#'
#' @param stanfit.object the estimated stan model output
#' @param cred.niveaus a vector of credibility niveaus/levels of the the equal-tailed and highest posterior density intervals
#'                    shall be calculated
#'
#' @return Information about the posterior distributions stored in a vector
#'         consisting of the following entry parts first for parameter nu followed by the same statistics regarding
#'         parameter gamma:
#'
#' \item{nu/ga.post.stats}{Summary statistics from the stanfit object about the posterior distribution, namely \code{mean, se_mean, sd},
#'                  and bayesian convergence diagnostic measures, namely \code{n_eff, Rhat}}
#' \item{nu/ga.eti}{equal-tailed posterior (credibility) intervals specified by their lower and upper boundaries;
#'                  one interval is derived for each specified credibility niveau}
#' \item{nu/ga.hdi}{highest posterior density intervals specified by their lower and upper boundaries;
#'                  one interval is derived for each specified credibility niveau}
#' \item{nu/ga.per}{percentiles for the posterior density}
#'
#'  @details The storing in one long vector for each parameter is provided to run a
#'  simulation study, where all relevant statistics per run are stored column-wise
#'  and the repeated runs for one data scenario are store row-wise.
#'
#' @export




# function --------------------------------------------------------------------
sim.stanfit.to.poststats = function(pc, stanfit.object, cred.niveaus = seq(0.5, 0.95, by = 0.05)){
  
  if(pc$tte.dist == "w"){
    obj = stanfit.object
    obj = stanfit.object
    
    # extract running time
    run.min = sum(rstan::get_elapsed_time(obj))/60 # in minutes
    names(run.min) = "run.min"
    
    # summary statistics for shape parameters
    post_summary = rstan::summary(obj, pars = c("nu"), probs = c())$summary
    poststats = list(nu = post_summary["nu",])
    
    nu.post.stats = poststats$nu
    names(nu.post.stats) = paste0("nu.po.", names(nu.post.stats))
    
    
    # extraction of posterior samples for credible interval calculation
    post.sample = rstan::extract(obj, pars = c("nu"))
    # results in
    # ## post.sample$nu = posterior sample of nu, representing the posterior distribution
    
    
    # calculate equal tailed intervals (ETIs)
    
    nu.eti = matrix(nrow = length(cred.niveaus), ncol = 2)
    for(i in 1:length(cred.niveaus)){
      nu.eti[i,] = stats::quantile(post.sample$nu, probs = 0.5 + c(-1,1)*cred.niveaus[i]/2)
    }
    nu.eti = c(t(nu.eti)) # transform to vector
    names(nu.eti) = paste0("nu.eti",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )
    
    
    # calculate highest density intervals (HDIs)
    
    nu.hdi = t(sapply(cred.niveaus, HDInterval::hdi, object = post.sample$nu))
    nu.hdi = c(t(nu.hdi)) # transform to vector
    names(nu.hdi) = paste0("nu.hdi",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )
    
    
    # calculate percentiles
    
    nu.per = stats::quantile(post.sample$nu, probs = (0:100)/100)
    names(nu.per) = paste0("nu.per", names(nu.per))
    
    
    # vector to be returned
    
    ret.vect = data.frame(t(c(run.min,
                              nu.post.stats, nu.eti, nu.hdi, nu.per)))
    
    ret.vect = cbind(pc, ret.vect)
    
  }
  
  if(pc$tte.dist == "dw"){
    obj = stanfit.object$uncens
    obj_c = stanfit.object$cens
    
    # extract running time
    
    run.min = sum(rstan::get_elapsed_time(obj) + rstan::get_elapsed_time(obj_c))/60 # in minutes
    names(run.min) = "run.min"
    
    # uncensored: 
    # summary statistics for shape parameter
    post_summary = rstan::summary(obj, pars = c("nu"), probs = c())$summary
    poststats = list(nu = post_summary["nu",])
    
    nu.post.stats = poststats$nu
    names(nu.post.stats) = paste0("nu.po.", names(nu.post.stats))
    
    
    # extraction of posterior samples for credible interval calculation
    post.sample = rstan::extract(obj, pars = c("nu"))
    
    # calculate equal tailed intervals (ETIs)
    
    nu.eti = matrix(nrow = length(cred.niveaus), ncol = 2)
    for(i in 1:length(cred.niveaus)){
      nu.eti[i,] = stats::quantile(post.sample$nu, probs = 0.5 + c(-1,1)*cred.niveaus[i]/2)
    }
    nu.eti = c(t(nu.eti)) # transform to vector
    names(nu.eti) = paste0("nu.eti",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )
    
    # calculate highest density intervals (HDIs)
    
    nu.hdi = t(sapply(cred.niveaus, HDInterval::hdi, object = post.sample$nu))
    nu.hdi = c(t(nu.hdi)) # transform to vector
    names(nu.hdi) = paste0("nu.hdi",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )
    
    # calculate percentiles
    
    nu.per = stats::quantile(post.sample$nu, probs = (0:100)/100)
    names(nu.per) = paste0("nu.per", names(nu.per))
    
    
    # censored: 
    # summary statistics for shape parameter
    post_summary = rstan::summary(obj_c, pars = c("nu"), probs = c())$summary
    poststats = list(ga = post_summary["nu",])
    
    ga.post.stats = poststats$ga
    names(ga.post.stats) = paste0("ga.po.", names(ga.post.stats))
    
    
    # extraction of posterior samples for credible interval calculation
    post.sample = rstan::extract(obj_c, pars = c("nu"))
    
    
    # calculate equal tailed intervals (ETIs)
    
    ga.eti = matrix(nrow = length(cred.niveaus), ncol = 2)
    for(i in 1:length(cred.niveaus)){
      ga.eti[i,] = stats::quantile(post.sample$nu, probs = 0.5 + c(-1,1)*cred.niveaus[i]/2)
    }
    ga.eti = c(t(ga.eti)) # transform to vector
    names(ga.eti) = paste0("ga.eti",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )
    
    
    # calculate highest density intervals (HDIs)
    
    ga.hdi = t(sapply(cred.niveaus, HDInterval::hdi, object = post.sample$nu))
    ga.hdi = c(t(ga.hdi)) # transform to vector
    names(ga.hdi) = paste0("ga.hdi",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )
    
    
    # calculate percentiles
    
    ga.per = stats::quantile(post.sample$nu, probs = (0:100)/100)
    names(ga.per) = paste0("ga.per", names(ga.per))
    
    
    
    # vector to be returned
    
    ret.vect = data.frame(t(c(run.min,
                              nu.post.stats, nu.eti, nu.hdi, nu.per,
                              ga.post.stats, ga.eti, ga.hdi, ga.per)))
    
    ret.vect = cbind(pc, ret.vect)
    
  }
  
  if(pc$tte.dist == "pgw"){
    obj = stanfit.object
    
    # extract running time
    
    run.min = sum(rstan::get_elapsed_time(obj))/60 # in minutes
    names(run.min) = "run.min"
    
    # summary statistics for shape parameters
    
    post_summary = rstan::summary(obj, pars = c("nu", "gamma"), probs = c())$summary
    poststats = list(nu = post_summary["nu",], ga = post_summary["gamma",])
    
    nu.post.stats = poststats$nu
    names(nu.post.stats) = paste0("nu.po.", names(nu.post.stats))
    
    ga.post.stats = poststats$ga
    names(ga.post.stats) = paste0("ga.po.", names(ga.post.stats))
    
    
    ######## extraction of posterior samples for credible interval calculation
    post.sample = rstan::extract(obj, pars = c("nu", "gamma"))
    
    # calculate equal tailed intervals (ETIs)
    nu.eti = matrix(nrow = length(cred.niveaus), ncol = 2)
    for(i in 1:length(cred.niveaus)){
      nu.eti[i,] = stats::quantile(post.sample$nu, probs = 0.5 + c(-1,1)*cred.niveaus[i]/2)
    }
    nu.eti = c(t(nu.eti)) # transform to vector
    names(nu.eti) = paste0("nu.eti",rep(cred.niveaus, each = 2), rep(c("l","u"), times = length(cred.niveaus)) )
    
    
    ga.eti = matrix(nrow = length(cred.niveaus), ncol = 2)
    for(i in 1:length(cred.niveaus)){
      ga.eti[i,] = stats::quantile(post.sample$gamma, probs = 0.5 + c(-1,1)*cred.niveaus[i]/2)
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
    
    nu.per = stats::quantile(post.sample$nu, probs = (0:100)/100)
    names(nu.per) = paste0("nu.per", names(nu.per))
    
    ga.per = stats::quantile(post.sample$gamma, probs = (0:100)/100)
    names(ga.per) = paste0("ga.per", names(ga.per))
    
    
    # vector to be returned
    
    ret.vect = data.frame(t(c(run.min,
                              nu.post.stats, nu.eti, nu.hdi, nu.per,
                              ga.post.stats, ga.eti, ga.hdi, ga.per)))
    
    ret.vect = cbind(pc, ret.vect)
  }

  rownames(ret.vect) = NULL
  return(ret.vect)
}



# ## test
# 
# # mod = inner mod object in sim.fit.to.1.sample fct
# post = sim.stanfit.to.poststats(pc = pc, stanfit.object = mod)
# View(post)

## END of Doc
