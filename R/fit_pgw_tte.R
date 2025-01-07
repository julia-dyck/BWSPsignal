#' Fit pgW models with stan
#'
#' The function applies the \code{rstan::sampling} command to fit a pgW model to 
#' time-to-event data with Gamma or Lognormal priors for the parameters of the
#' pgW distribution.
#'
#' @param datstan A named list of data for the stan model; a dataframe can be 
#' reformated with \code{tte2standat()}.
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
#' Additional details...
#' 
#'
#' @export


fit_pgw_tte = function(datstan, 
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


