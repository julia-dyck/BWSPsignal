#' Fit Bayesian Power generalized Weibull models to time-to-event data
#'
#' 
#' The function applies the \code{\link[rstan]{sampling}} command to fit a Power
#' generalized Weibull (pgw) model to 
#' time-to-event  (tte) data with Gamma or Lognormal priors for the parameters of the
#' pgW distribution.
#'
#' @param datstan A named list of data for the stanmodel; a tte data frame can be 
#' reformated with \code{\link{tte2priordat_pgw}}.
#' @param priordist A character string indicating the prior distribution for the
#' parameters of the pgW distribution. Options are 
#' \code{"fgg", "fll", "ggg", "lll"} (see details).
#' @param chains The number of Markov chains to run
#' @param iter The total number of iterations per chain (including warmup)
#' @param warmup The number of warmup iterations per chain
#' 
#' 
#' @return A stanfit object
#' 
#' 
#' @details
#' The function applies the \code{\link[rstan]{sampling}} command to fit a Bayesian pgw model to 
#' tte data.
#' 
#' The posterior is proportional to the likelihood times the prior. The likelihood is
#' \deqn{\mathcal{L}(t| \Theta) = \prod_{i=1}^N S(t_i)^{1-d_i}\cdot f(t_i)^{d_i}} 
#' with \eqn{S(t)} the survival function of the pgW distribution and \eqn{f(t)} the
#' density function of the pgW distribution. The pair \eqn{(t_i, d_i)} are the observed
#' tte observations.
#' 
#' The priors are either independent univariate Gamma or Lognormal distribution
#' for the parameters of the pgW distribution. 
#' Implemented distributional choices for the joint prior are products of the following:
#' \tabular{llll}{
#' for scale  \tab for shape  \tab for powershape \tab abbreviation \cr
#' fixed to prior mean \tab Gamma \tab Gamma \tab fgg \cr
#' Gamma \tab Gamma \tab Gamma \tab ggg \cr
#' fixed to prior mean \tab Lognormal \tab Lognormal \tab fll \cr
#' Lognormal \tab Lognormal \tab Lognormal \tab lll \cr
#' }
#' 
#' @examples
#' # prep the data
#' head(tte)
#' standat = tte2priordat_pgw(dat = tte,
#'                            scale.mean = 1, 
#'                            scale.sd = 10,
#'                            shape.mean = 1, 
#'                            shape.sd = 10,
#'                            powershape.mean = 1, 
#'                            powershape.sd = 10)
#' # fit the model
#' fit = fit_tte_pgw(datstan = standat,  # (be aware that posterior sample
#'                  priordist = "lll",   # is small for demo purpose)
#'                  chains = 4,
#'                  iter = 110,
#'                  warmup = 10)
#' # print the summary
#' fit
#' 
#' @export
#'


fit_tte_pgw = function(datstan, 
                          priordist = c("fgg","fll","ggg","lll"),
                          chains = 4,
                          iter = 11000,
                          warmup = 1000
                          ){
  if(priordist == "fgg"){
    output = rstan::sampling(
      object = stanmodels$pgw_tte_gammaprior_scalefixed,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  if(priordist == "fll"){
    output = rstan::sampling(
      object = stanmodels$pgw_tte_lognormalprior_scalefixed,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  if(priordist == "ggg"){
    output = rstan::sampling(
      object = stanmodels$pgw_tte_gammaprior,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  if(priordist == "lll"){
    output = rstan::sampling(
      object = stanmodels$pgw_tte_lognormalprior,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  return(output)
}


