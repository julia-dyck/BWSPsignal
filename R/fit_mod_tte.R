#' Fit stan model to time-to-event data
#'
#' Fits a Bayesian model to time-to-event data
#' for the purpose of performing signal detection tests with \code{\link{bwsp_test}}
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
#' @param tte.dist A character string indicating the modelling approach. Options are
#' \code{"w", "dw", "pgw"}.
#' @param prior.dist A character string indicating the prior distribution for the
#' parameters of the pgW distribution. Options are 
#' \code{"fg", "fl", "gg", "ll"} (see details).
#' @param chains The number of Markov chains to run
#' @param iter The total number of iterations per chain (including warmup)
#' @param warmup The number of warmup iterations per chain
#' 
#' 
#' @return A stanfit object; in case of \code{tte.dist = "dw"} a list of two stanfit objects
#' with \code{$uncens} containing the stanfit object
#' obtained from the model to the tte data as is, and \code{$cens} containing the stanfit object
#' obtained from the model to the tte data censored at mid of observation period.
#' 
#' 
#' @details
#' The function applies the \code{\link[rstan]{sampling}} command to fit a Bayesian model to 
#' time-to-event data.
#' The model can be a Weibull,
#' a double Weibull (estimating two Weibull models - one to the data as is & 
#' one to the data censored at half of the observation period), 
#' or a power generalized Weibull distribution.
#' 
#' The posterior is proportional to the likelihood times the prior. The likelihood is
#' \deqn{\mathcal{L}(t| \Theta) = \prod_{i=1}^N S(t_i)^{1-d_i}\cdot f(t_i)^{d_i}} 
#' with \eqn{S(t)} the survival function of the chosen distribution and \eqn{f(t)} the
#' density function. The pair \eqn{(t_i, d_i)} are the observed
#' time-to-event observations.
#' 
#' Implemented prior distributions for the scale and shape parameters are products 
#' of the following univariate distributional choices:
#' \tabular{lll}{
#' for scale parameter(s) \eqn{\theta} \tab for shape parameter(s) \tab  abbreviation \cr
#' fixed to prior mean \tab Gamma  \tab fg \cr
#' Gamma \tab Gamma \tab gg \cr
#' fixed to prior mean \tab Lognormal \tab fl \cr
#' Lognormal \tab Lognormal \tab ll \cr
#' }
#' 
#' @examples
#' # prep the data
#' head(tte)
#' standat = tte2standat(dat = tte,
#'                      tte.dist = "pgw",
#'                      scale.mean = 1, 
#'                      scale.sd = 10,
#'                      shape.mean = 1, 
#'                      shape.sd = 10,
#'                      powershape.mean = 1, 
#'                      powershape.sd = 10)
#' # fit a pgw model
#' fit = fit_mod_tte(datstan = standat,  
#'                   tte.dist = "pgw",         
#'                   prior.dist = "ll",  
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
                       tte.dist = c("w", "dw", "pgw"),
                       prior.dist = c("fg","fl","gg","ll"),
                       chains = 4,
                       iter = 11000,
                       warmup = 1000){
  if(mod == "w"){
    fit = fit_w_tte(datstan = datstan,
                    prior.dist = prior.dist,
                    chains = chains,
                    iter = iter,
                    warmup = warmup)
  }
  else if(mod == "dw"){
    fit = fit_dw_tte(datstan = datstan,
                     prior.dist = prior.dist,
                     chains = chains,
                     iter = iter,
                     warmup = warmup)
  }
  else if(mod == "pgw"){
    fit = fit_pgw_tte(datstan = datstan,
                      prior.dist = prior.dist,
                      chains = chains,
                      iter = iter,
                      warmup = warmup)
  }
  else{
    stop("Argument tte.dist must be one of 'w', 'dw', or 'pgw'.")
  }
  return(fit)
}



