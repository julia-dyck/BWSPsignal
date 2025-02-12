#' Fit stan model to time-to-event data
#'
#' Fits a Bayesian model to time-to-event data
#' for the purpose of performing signal detection tests with \code{\link{bwsptest}}
#' based on the posterior samples of the shape parameters of the chosen model
#' distribution.
#' 
#' The model can be a Weibull,
#' a double Weibull ( estimating two Weibull models - one to the data as is & 
#' one to the data censored at mid of observation period), 
#' or a power generalized Weibull distribution.
#'
#' @param datstan A named list of data for the stanmodel; a data frame can be 
#' reformated with \code{\link{tte2standat}}.
#' @param mod A character string indicating the modelling approach. Options are
#' \code{"w", "dw", "pgw"}.
#' @param priordist A character string indicating the prior distribution for the
#' parameters of the pgW distribution. Options are 
#' \code{"fgg", "fll", "ggg", "lll"} (see details).
#' @param chains The number of Markov chains to run
#' @param iter The total number of iterations per chain (including warmup)
#' @param warmup The number of warmup iterations per chain
#' 
#' 
#' @return A stanfit object; in case of \code{mod = "dw"} a list of two stanfit objects
#' with \code{$uncens} containing the stanfit object
#' obtained from the model to the tte data as is, and \code{$cens} containing the stanfit object
#' obtained from the model to the tte data censored at mid of observation period.
#' 
#' 
#' @details
#' The function applies the \code{\link[rstan]{sampling}} command to fit a Bayesian model to 
#' time-to-event data.
#' The model can be a Weibull,
#' a double Weibull ( estimating two Weibull models - one to the data as is & 
#' one to the data censored at mid of observation period), 
#' or a power generalized Weibull distribution.
#' 
#' The posterior is proportional to the likelihood times the prior. The likelihood is
#' \deqn{\mathcal{L}(t| \Theta) = \prod_{i=1}^N S(t_i)^{1-d_i}\cdot f(t_i)^{d_i}} 
#' with \eqn{S(t)} the survival function of the chosen distribution and \eqn{f(t)} the
#' density function. The pair \eqn{(t_i, d_i)} are the observed
#' time-to-event observations.
#' 
#' The priors are either independent univariate Gamma or Lognormal distribution
#' for the parameters of the pgW distribution. 
#' Implemented distributional choices for the joint prior are products of the following:
#' \tabular{llll}{
#' for scale \eqn{\theta} \tab for shape \eqn{\nu} \tab for powershape \eqn{\gamma} \tab abbreviation \cr
#' fixed to prior mean \tab Gamma \tab Gamma \tab fgg \cr
#' Gamma \tab Gamma \tab Gamma \tab ggg \cr
#' fixed to prior mean \tab Lognormal \tab Lognormal \tab fll \cr
#' Lognormal \tab Lognormal \tab Lognormal \tab lll \cr
#' }
#' 
#' @examples
#' # prep the data
#' head(tte)
#' standat = tte2standat(dat = tte,
#'                      mod = "pgw",
#'                      scale.mean = 1, 
#'                      scale.sd = 10,
#'                      shape.mean = 1, 
#'                      shape.sd = 10,
#'                      powershape.mean = 1, 
#'                      powershape.sd = 10)
#' # fit a pgw model
#' fit = fit_pgw_tte(datstan = standat,  
#'                   mod = "pgw",         
#'                   priordist = "lll",  
#'                   chains = 4,
#'                   iter = 110,          # (be aware that posterior sample
#'                   warmup = 10)         # is small for demo purpose)
#'                   
#' # print the summary
#' fit
#' 
#' 
#'
#' @export
#'
#'

fit_mod_tte = function(datstan, 
                       mod = c("w", "dw", "pgw"),
                       priordist = c("fgg","fll","ggg","lll"),
                       chains = 4,
                       iter = 11000,
                       warmup = 1000){
  if(mod == "w"){
    fit = fit_w_tte(datstan = datstan,
                    priordist = priordist,
                    chains = chains,
                    iter = iter,
                    warmup = warmup)
  }
  else if(mod == "dw"){
    fit = fit_dw_tte(datstan = datstan,
                     priordist = priordist,
                     chains = chains,
                     iter = iter,
                     warmup = warmup)
  }
  else if(mod == "pgw"){
    fit = fit_pgw_tte(datstan = datstan,
                      priordist = priordist,
                      chains = chains,
                      iter = iter,
                      warmup = warmup)
  }
  else{
    stop("Argument mod must be one of 'w', 'dw', or 'pgw'.")
  }
  return(fit)
}



