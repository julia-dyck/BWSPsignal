#' Fit Bayesian Weibull models to time-to-event data
#'
#' 
#' The function applies the \code{\link[rstan]{sampling}} command to fit a Weibull (w) model to 
#' time-to-event  (tte) data with Gamma or Lognormal priors for the parameters of the
#' pgW distribution.
#'
#' @param datstan A named list of data for the stanmodel; a data frame can be 
#' reformated with \code{\link{tte2priordat_w}}.
#' @param prior.dist A character string indicating the prior distribution for the
#' parameters of the pgW distribution. Options are 
#' \code{"fg", "fl", "gg", "ll"} (see details).
#' @param chains The number of Markov chains to run
#' @param iter The total number of iterations per chain (including warmup)
#' @param warmup The number of warmup iterations per chain
#' 
#' 
#' @return A stanfit object
#' 
#' 
#' @details
#' The posterior is proportional to the likelihood times the prior. The likelihood is
#' \deqn{\mathcal{L}(t| \Theta) = \prod_{i=1}^N S(t_i)^{1-d_i}\cdot f(t_i)^{d_i}} 
#' with \eqn{S(t)} the survival function of the Weibull distribution and \eqn{f(t)} the
#' density function of the Weibull distribution. The pair \eqn{(t_i, d_i)} are the observed
#' time-to-event observations.
#' 
#' The priors are either independent univariate Gamma or Lognormal distribution
#' for the parameters of the pgW distribution. 
#' Implemented distributional choices for the joint prior are products of the following:
#' \tabular{llll}{
#' for scale  \tab for shape \tab abbreviation \cr
#' fixed to prior mean \tab Gamma  \tab fg \cr
#' Gamma \tab Gamma \tab gg \cr
#' fixed to prior mean \tab Lognormal \tab fl \cr
#' Lognormal \tab Lognormal \tab ll \cr
#' }
#' 
#' @examples
#' # prep the data
#' head(tte)
#' standat = tte2priordat_w(dat = tte,
#'                          scale.mean = 1, 
#'                          scale.sd = 10,
#'                          shape.mean = 1, 
#'                          shape.sd = 10)
#' # fit the model
#' fit = bwsp_model_w(datstan = standat,  # (be aware that posterior sample
#'                  prior.dist = "ll",   # is small for demo purpose)
#'                  chains = 4,
#'                  iter = 110,
#'                  warmup = 10)
#' # print the summary
#' fit
#' 
#' @export
#'


bwsp_model_w = function(datstan, 
                     prior.dist = c("fg","fl","gg","ll"),
                     chains = 4,
                     iter = 11000,
                     warmup = 1000
){
  if(prior.dist == "fg"){
    output = rstan::sampling(
      object = stanmodels$w_tte_gammaprior_scalefixed,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  if(prior.dist == "fl"){
    output = rstan::sampling(
      object = stanmodels$w_tte_lognormalprior_scalefixed,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  if(prior.dist == "gg"){
    output = rstan::sampling(
      object = stanmodels$w_tte_gammaprior,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  if(prior.dist == "ll"){
    output = rstan::sampling(
      object = stanmodels$w_tte_lognormalprior,  # Stan model
      data = datstan,     # named list of data
      chains = chains,    # number of Markov chains
      warmup = warmup,    # number of warmup iterations per chain
      iter = iter         # total number of iterations per chain (including warmup)
    )
  }
  return(output)
}


