#' Bayesian Weibull Shape Parameter Test
#' 
#' @description 
#' Combined HDI+ROPE test for the shape parameters of the pgW distribution.
#' 
#'
#' @param credregions vector of length 4 with the lower and upper boundaries of the 
#' credibility interval (CI) reflecting the posterior distribution of the pgW shape
#' parameters \eqn{\nu} and \eqn{\gamma}; required order: 1. lower boundary of
#' CI(\eqn{\nu}) , 2. upper boundary of CI(\eqn{\nu}) , 3. lower boundary of
#' CI(\eqn{\gamma}) , 4. upper boundary of CI(\eqn{\gamma})
#' @param nullregion a vector of length two denoting the lower and upper boundary of the ROPE
#' @param option rule to be used to combine single parameter test results
#' @param mod modelling approach used to obtain the posterior samples of the shape
#' parameters; default and currently only option is "pgw"
#' 
#' @return 0 if \eqn{H_0} is accepted, 1 if \eqn{H_1} is rejected; see details for definition
#' of \eqn{H_0} and \eqn{H_1}
#'
#'
#'
#' @section Test concept: 
#' 
#' The Bayesian Weibull shape parameter test is a hypothesis test 
#' for signal detection of adverse drug reactions.
#' It is based on the principle of non-constant hazard function \insertCite{cornelius2012}{BWSPsignal}
#' that can be formalized as the following hypotheses \insertCite{sauzet2024}{BWSPsignal}
#' depending on the underlying model:
#' 
#' \tabular{lcc}{
#'         \tab \eqn{H_0} \tab \eqn{H_1} \cr
#'  general formulation \tab constant hazard function \tab non-constant hazard function \cr
#'  under pgW model \tab \eqn{\nu = 1 \text{ and } \gamma = 1} \tab \eqn{\nu \neq 1 \text{ or } \gamma \neq 1} \cr
#'  under Weibull model \tab \eqn{\nu = 1} \tab \eqn{\nu \neq 1} \cr
#'  (not implemented yet) \tab \tab \cr
#'  under double Weibull model \tab \eqn{\nu_1 = 1 \text{ and } \nu_2 = 1} \tab \eqn{\nu \neq 1 \text{ or } \gamma \neq 1} \cr
#'  (not implemented yet) \tab \tab \cr
#' }
#' 
#' 
#' 
#' @section Bayesian approach: 
#' 
#' More extensive information on the Bayesion Power Generalized Weibull shape 
#' parameter test approach can be found in \insertCite{dyck2024bpgwsppreprint}{BWSPsignal}.
#' 
#' The region of practical equivalence (ROPE) specified in the 
#' \code{nullregion} argument represents the expected parameter value under \eqn{H_0}.
#' The credibility regions represent the posterior distribution of each shape parameter.
#' 
#' The BWSP test conducts an HDI+ROPE test (see \code{\link{hdi_plus_rope()}}) for each 
#' shape parameter and combines the interim results of all shape parameters. 
#' 
#' @section Options for combination rule: 
#' The formalized hypotheses under a model with two parameters defines 
#' one intuitive combination rule (\code{option = 1}). Two addition, more reserved options  
#' ( ie. leading to a signal in fewer cases) are implemented:
#'
#' \tabular{ccccc}{
#' HDI+ROPE \tab HDI+ROPE \tab combination \tab combination \tab combination \cr
#' outcome \tab outcome \tab rule \tab rule \tab rule \cr
#' for \eqn{\nu}\tab for \eqn{\gamma} \tab (\code{option = 1}) \tab (\code{option = 2}) \tab (\code{option = 3}) \cr
#' rejection \tab rejection \tab signal \tab signal \tab signal \cr
#' acceptance \tab rejection \tab signal \tab - \tab - \cr
#' rejection \tab acceptance \tab signal \tab - \tab - \cr
#' acceptance \tab acceptance \tab - \tab - \tab - \cr
#' no decision \tab rejection \tab signal \tab signal \tab - \cr
#' no decision \tab acceptance \tab - \tab - \tab - \cr
#' rejection \tab no decision \tab signal \tab signal \tab - \cr 
#' acceptance \tab no decision \tab - \tab - \tab - \cr
#' no decision \tab no decision \tab signal \tab - \tab - \cr
#' }
#' 
#' @references 
#' \insertAllCited{}
#' 
#' 
#' @export
#'


bwsp_test = function(credregions, nullregion, option = c(1,2,3), mod = "pgw"){
  # later: add mod arguments c("pgW", "W", "dW") for posteriors obtained with
  # either the pgw, the Weibull or the double Weibull modelling approach (see 
  # Sauzet 2024 for that)
  nu.credregion = credregions[1:2]
  ga.credregion = credregions[3:4]

  nu.ropehdi_res = hdi_plus_rope(nullregion = nullregion, credregion = nu.credregion)
  ga.ropehdi_res = hdi_plus_rope(nullregion = nullregion, credregion = ga.credregion)
  res = c(nu.ropehdi_res, ga.ropehdi_res)

  if(option == 1){
    if(sum(is.na(res)) == 2){ # both undecided
      return(1)
    }
    
    if(sum(is.na(res)) == 1){ # one undecided, other leads the combined result
      out = res[!is.na(res)]
      return(out)
    }
    
    if(sum(is.na(res)) == 0){ # both decided
      out = ifelse(sum(res) == 0, 0, 1)
      return(out)
    }
  }
  if(option == 2){
    if(sum(is.na(res)) == 2){ # both undecided
      return(0)
    }
    
    if(sum(is.na(res)) == 1){ # one undecided, other leads the combined result
      out = res[!is.na(res)]
      return(out)
    }
    
    if(sum(is.na(res)) == 0){ # both decided
      out = ifelse(sum(res) == 2, 1, 0)
      return(out)
    }
  }
  if(option == 3){
    if(sum(is.na(res)) == 2){ # both undecided
      return(0)
    }
    
    if(sum(is.na(res)) == 1){ # one undecided
      return(0)
    }
    
    if(sum(is.na(res)) == 0){ # both decided
      out = ifelse(sum(res) == 2, 1, 0)
      return(out)
    }
  }
  else{
    stop("option must be 1, 2 or 3")
  
  }
}




## END OF DOC