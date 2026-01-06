#' Fit Bayesian model to time-to-event data
#'
#' Fits a Bayesian model to time-to-event (tte) data
#' for the purpose of performing Weibull shape parameter signal detection tests 
#' with \code{\link{bwsp_test}}.
#' 
#'
#' @param datstan named list of data for the stanmodel; tte data can be 
#' formated with \code{\link{tte2priordat}}
#' @param tte.dist character indicating the modelling approach; options are
#' \code{"w", "dw", "pgw"} (see details)
#' @param prior.dist character indicating the prior distribution for the
#' parameters of the tte distribution; options are 
#' \code{"fg", "fl", "gg", "ll"} (see details)
#' @param chains number of Markov chains to run
#' @param iter total number of iterations per chain (including warmup)
#' @param warmup number of warmup iterations per chain
#' 
#' 
#' @return A stanfit object or, in case of \code{tte.dist = "dw"}, a list of two stanfit objects
#' with \code{$uncens} containing the stanfit object
#' obtained from the model to the tte data as is, and \code{$cens} containing the stanfit object
#' obtained from the model to the tte data censored at mid of observation period.
#' 
#' 
#' @details 
#' The function applies the \code{\link[rstan]{sampling}} command with the No U-Turn sampler
#' to fit a Bayesian model to 
#' time-to-event data.
#' The model can be a Weibull \code{("w")},
#' a double Weibull \code{("dw"}, estimating two Weibull models - one to the data as is and 
#' one to the data censored at mid of observation period), 
#' or a power generalized Weibull \code{("pgw")} model.
#' 
#' The posterior is proportional to the likelihood times the prior. The likelihood is
#' \deqn{\mathcal{L}(t| \Theta) = \prod_{i=1}^N S(t_i)^{1-d_i}\cdot f(t_i)^{d_i}} 
#' with \eqn{S(t)} the survival function of the chosen distribution and \eqn{f(t)} the
#' density function \insertCite{nikulin2016}{WSPsignal}. The pair \eqn{(t_i, d_i)} are the tte observations.
#' 
#' Implemented prior distributions for the scale and shape parameters are products 
#' of the following univariate distributional choices:
#' \tabular{lll}{
#' for scale parameter \tab for shape parameter(s) \tab  abbreviation \cr
#' fixed to prior mean \tab gamma  \tab fg \cr
#' gamma \tab gamma \tab gg \cr
#' fixed to prior mean \tab lognormal \tab fl \cr
#' lognormal \tab lognormal \tab ll \cr
#' }
#' 
#' @references 
#' \insertAllCited{}
#' 
#' @examples
#' # prep the data
#' head(tte)
#' standat = tte2priordat(dat = tte,
#'                      tte.dist = "pgw",
#'                      scale.mean = 1, 
#'                      scale.sd = 10,
#'                      shape.mean = 1, 
#'                      shape.sd = 10,
#'                      powershape.mean = 1, 
#'                      powershape.sd = 10)
#' # fit a pgw model
#' fit = bwsp_model(datstan = standat,  
#'                   tte.dist = "pgw",         
#'                   prior.dist = "ll",  
#'                   chains = 4,
#'                   iter = 110,          # (posterior sample
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

bwsp_model = function(datstan, 
                       tte.dist = c("w", "dw", "pgw"),
                       prior.dist = c("fg","fl","gg","ll"),
                       chains = 4,
                       iter = 11000,
                       warmup = 1000){
  if(tte.dist == "w"){
    fit = bwsp_model_w(datstan = datstan,
                    prior.dist = prior.dist,
                    chains = chains,
                    iter = iter,
                    warmup = warmup)
  }
  else if(tte.dist == "dw"){
    fit = bwsp_model_dw(datstan = datstan,
                     prior.dist = prior.dist,
                     chains = chains,
                     iter = iter,
                     warmup = warmup)
  }
  else if(tte.dist == "pgw"){
    fit = bwsp_model_pgw(datstan = datstan,
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



