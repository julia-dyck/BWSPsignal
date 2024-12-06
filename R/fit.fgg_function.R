#' fit prespecified pgw stan models
#'
#' The functions essentially just apply the \code{rstan::stan} command with an already prespecified .stan file.
#' Within a simulation study, the function can be run in parallel on various prior meta-parameter specifications or datasets.
#'
#'
#'
#' @export


fit.fgg = function(datstan, cores){
  output = rstan::stan(
    file = "stanfiles/pgw_tte_gammaprior_scalefixed.stan",  # Stan program
    model_name = "pgw_tte_gammaprior_scalefixed_model", # model name
    data = datstan,         # named list of data
    chains = 4,             # number of Markov chains
    warmup = 1000,          # number of warmup iterations per chain
    iter = 11000,            # total number of iterations per chain (including warmup)
    cores = cores,          # number of cores (one per chain)
    refresh = -1            # progress not shown
  )
  return(output)
}



#' fit prespecified pgw stan models
#'
#' The functions essentially just apply the \code{rstan::stan} command with an already prespecified .stan file.
#' Within a simulation study, the function can be run in parallel on various prior meta-parameter specifications or datasets.
#'
#'
#' @export



fit.ggg = function(datstan, cores){
  output = rstan::stan(
  file = "stanfiles/pgw_tte_gammaprior.stan",  # Stan program
  model_name = "pgw_tte_gammaprior_model", # model name
  data = datstan,         # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 11000,            # total number of iterations per chain (including warmup)
  cores = cores,          # number of cores (one per chain)
  refresh = -1            # progress not shown
  )
  return(output)
}

#' fit prespecified pgw stan models
#'
#' The functions essentially just apply the \code{rstan::stan} command with an already prespecified .stan file.
#' Within a simulation study, the function can be run in parallel on various prior meta-parameter specifications or datasets.
#'
#'
#' @export



fit.fll = function(datstan, cores){
  output = rstan::stan(
    file = "stanfiles/pgw_tte_lognormalprior_scalefixed.stan",  # Stan program
    model_name = "pgw_tte_lognormalprior_scalefixed_model", # model name
    data = datstan,         # named list of data
    chains = 4,             # number of Markov chains
    warmup = 1000,          # number of warmup iterations per chain
    iter = 11000,            # total number of iterations per chain (including warmup)
    cores = cores,          # number of cores (one per chain)
    refresh = -1            # progress not shown
  )
  return(output)
}

#' fit prespecified pgw stan models
#'
#' The functions essentially just apply the \code{rstan::stan} command with an already prespecified .stan file.
#' Within a simulation study, the function can be run in parallel on various prior meta-parameter specifications or datasets.
#'
#'
#'
#' @export



fit.lll = function(datstan, cores){
  output = rstan::stan(
    file = "stanfiles/pgw_tte_lognormalprior.stan",  # Stan program
    model_name = "pgw_tte_lognormalprior_model", # model name
    data = datstan,   # named list of data
    chains = 4,       # number of Markov chains
    warmup = 1000,    # number of warmup iterations per chain
    iter = 11000,     # total number of iterations per chain (including warmup)
    cores = cores,    # number of cores (one per chain)
    refresh = -1      # progress not shown
  )
  return(output)
}

