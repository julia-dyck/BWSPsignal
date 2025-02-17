#' Bayesian Weibull Shape Parameter Test
#' 
#' @description 
#' Bayesian hypothesis test based on the shape parameters of a distribution of the 
#' Weibull family.
#' 
#'
#' @param credregions vector of length 2 or 4 with the lower and upper boundaries of the 
#' credibility interval (CI) reflecting the posterior distribution of the shape
#' parameter(s); required order: 1. lower CI boundary of first shape parameter, 
#' 2. upper CI boundary of first shape parameter; and if existent: 3. lower CI boundary of second shape parameter, 
#' 4. upper CI boundary of second shape parameter
#' @param nullregion a vector of length two denoting the lower and upper boundary of the region of practical equivalence (ROPE)
#' @param option numeric value out of \code{1,2,3}; rule to be used to combine single parameter test results
#' @param mod a character out of \code{"w", "dw", "pgw"}; specifies the modelling 
#' approach used to obtain the posterior samples of the shape parameter(s)
#' 
#' 
#' @return 0 if \eqn{H_0} is accepted, 1 if \eqn{H_1} is rejected; see details for definition
#' of \eqn{H_0} and \eqn{H_1}
#'
#'
#'
#' @section Test concept: 
#' 
#' The Bayesian Weibull shape parameter (WSP) test is a hypothesis test 
#' for signal detection of adverse drug reactions.
#' It is based on the principle of non-constant hazard function \insertCite{cornelius2012}{BWSPsignal}
#' that can be formalized as the following hypotheses \insertCite{sauzet2024}{BWSPsignal}
#' depending on the underlying model:
#' 
#' \tabular{lcc}{
#'         \tab \eqn{H_0} \tab \eqn{H_1} \cr
#'  general formulation \tab constant hazard function \tab non-constant hazard function \cr
#'  under Weibull model \tab \eqn{\nu = 1} \tab \eqn{\nu \neq 1} \cr
#'  under double Weibull model \tab \eqn{\nu_1 = 1 \text{ and } \nu_2 = 1} \tab \eqn{\nu \neq 1 \text{ or } \gamma \neq 1} \cr
#'  under pgW model \tab \eqn{\nu = 1 \text{ and } \gamma = 1} \tab \eqn{\nu \neq 1 \text{ or } \gamma \neq 1} \cr
#' }
#' 
#' 
#' 
#' @section Bayesian approach: 
#' 
#' Information on the Bayesian 
#' variant of the Power Generalized Weibull shape 
#' parameter test can be found in \insertCite{dyck2024bpgwsppreprint}{BWSPsignal}.
#' The same concept applies to the construction of the Bayesian Weibull and double
#' Weibull shape parameter test.
#' 
#' The region of practical equivalence (ROPE) specified in the 
#' \code{nullregion} argument represents the expected parameter value under \eqn{H_0}.
#' The credibility region(s) specified in the \code{credregions} argument represent
#' the posterior distribution of each shape parameter.
#' 
#' The BWSP test conducts an HDI+ROPE test (see \code{\link{hdi_plus_rope}}) for each 
#' shape parameter and combines the interim results of all shape parameters to a binary outcome. 
#' More information on the HDI+ROPE test and recommendations for interval specifications
#' can be found in \insertCite{kruschke2018}{BWSPsignal} and
#' \insertCite{dyck2024bpgwsppreprint}{BWSPsignal}.
#' 
#' @section Options for combination rule: 
#' HDI+ROPE testing (see \code{\link{hdi_plus_rope}}) can lead to acceptance of \eqn{H_0}, 
#' rejection of \eqn{H_0} or no decision. 
#' 
#' Options to generate a binary outcome, ie. a signal or not, from HDI+ROPE test results 
#' based on one (in case of \code{mod = "w"}) ore two shape parameters are:
#' \tabular{ccccc}{
#' HDI+ROPE \tab HDI+ROPE \tab combination \tab combination \tab combination \cr
#' outcome \tab outcome \tab rule \tab rule \tab rule \cr
#' for shape_1\tab for shape_2 \tab (\code{option = 1}) \tab (\code{option = 2}) \tab (\code{option = 3}) \cr
#' rejection \tab (none) \tab signal \tab signal \tab signal \cr
#' acceptance \tab (none) \tab - \tab - \tab - \cr
#' no decision \tab (none) \tab signal \tab - \tab - \cr
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
#' The hypotheses stated above are directly implemented in \code{option = 1}.
#' \code{option = 2} and \code{option = 3} are more reserved ie. lead to a signal in fewer cases.
#' 
#' @references 
#' \insertAllCited{}
#' 
#' 
#' @examples
#' #### Exemplary conduction of a test from data and prior to test result:
#' 
#' # under weibull model:
#' 
#' # 1. specify ROPE reflecting the null hypothesis:
#' # we choose an 80% confidence interval around the 
#' # null value (1 for both shape parameters)
#' logpars = logprior_repar(m_t = 1,s_t = 10) # get parameters of a 
#'                                            # Lognormal distribution with 
#'                                            # mean 1 and sd 10
#' 
#' rope = qlnorm(p = c(0.1,0.9), meanlog = logpars[1], sdlog = logpars[2])
#' 
#' # 2. Prior specification and model fitting:
#' # we formalize a prior belief (here "no association
#' # between drug and event", therefore prior mean = 1 for shape parameter)
#' # and reformat our tte data to fit the model in the following
#' standat = tte2standat(dat = tte,   # reformat the data
#'                       mod = "w",
#'                       scale.mean = 1, 
#'                       scale.sd = 10,
#'                       shape.mean = 1, 
#'                       shape.sd = 10)
#' 
#' fit = fit_mod_tte(datstan = standat,      # fit the model
#'                   mod = "w",
#'                   priordist = "lll",       
#'                   chains = 4,              
#'                   iter = 110,             # (be aware that posterior sample
#'                   warmup = 10)            # is small for demo purpose)
#' 
#' # 3. HDI specification and extraction:
#' # extract 80% HDIs representing posterior samples of the shape parameters
#' post.sample = rstan::extract(fit, pars = c("nu"))
#' nu.hdi = HDInterval::hdi(object = post.sample$nu, credMass = 0.8)
#' 
#' # 4. conduct the BWSPtest
#' bwsp_test(credregions = nu.hdi, 
#'           nullregion = rope, 
#'           mod = "w", 
#'           option = 1)
#' 
#' # under pgw model:
#' 
#' # 1. specify ROPE reflecting the null hypothesis:
#' # we choose an 80% confidence interval around the 
#' # null value (1 for both shape parameters)
#' 
#' logpars = logprior_repar(m_t = 1,s_t = 10) # get parameters of a 
#' # Lognormal distribution with 
#' # mean 1 and sd 10
#' 
#' rope = qlnorm(p = c(0.1,0.9), meanlog = logpars[1], sdlog = logpars[2])
#' 
#' # 2. Prior specification and model fitting:
#' # we formalize a prior belief (here "no association
#' # between drug and event", therefore prior mean = 1 for both shape parameters)
#' # and reformat our tte data to fit the model in the following
#' standat = tte2standat(dat = tte,          # reformat the data
#'                       mod = "pgw",
#'                       scale.mean = 1, 
#'                       scale.sd = 10,
#'                       shape.mean = 1, 
#'                       shape.sd = 
#'                         10,
#'                       powershape.mean = 1, 
#'                       powershape.sd = 10)
#' 
#' fit = fit_mod_tte(datstan = standat,     # fit the model
#'                   mod = "pgw",
#'                   priordist = "lll",       
#'                   chains = 4,              
#'                   iter = 110,            # (be aware that posterior sample
#'                   warmup = 10)           # is small for demo purpose)
#' 
#' # 3. HDI specification and extraction:
#' # extract 80% HDIs representing posterior samples of the shape parameters
#' post.samples = rstan::extract(fit, pars = c("nu", "gamma")) 
#' nu.hdi = HDInterval::hdi(object = post.samples$nu, credMass = 0.8)
#' ga.hdi = HDInterval::hdi(object = post.samples$gamma, credMass = 0.8)
#' 
#' # 4. conduct the BWSP test
#' bwsp_test(credregions = c(nu.hdi, ga.hdi), 
#'           nullregion = rope, 
#'           mod = "pgw", 
#'           option = 1)
#' 
#' # returns a signal
#' 
#' 
#' @export
#'


bwsp_test = function(credregions, 
                     nullregion, 
                     mod = c("w", "dw", "pgw"),
                     option = c(1,2,3)){
  # argument check for nullregion
  if(length(nullregion) != 2){
    stop("Argument nullregion must be a vector of length 2.")
  }
  # argument check for mod
  if(mod != "w" & mod != "dw" & mod != "pgw"){
    stop("mod must be one of 'w', 'dw' or 'pgw'")
  }
  # argument check for option 
  if(option != 1 & option != 2 & option != 3){
    stop("option must be one of 1, 2 or 3")
  }
  
  # test under Weibull model
  if(mod == "w"){
    # argument check for credregions
    if(length(credregions) != 2){
      stop("Argument credregions must be a vector of length 2.")
    }
    shape1credregion = credregions
    shape1ropehdi_res = hdi_plus_rope(nullregion = nullregion, credregion = shape1credregion)
    res = shape1ropehdi_res
    # options for "w" model
    if(option == 1){ # HDI+ROPE test rejects H0 or undecided -> signal
      out = ifelse(is.na(res), 1, res)
      return(out)
    }
    if(option == 2 | option == 3){ # HDI+ROPE test rejects H0 -> signal
      out = ifelse(is.na(res), 0, res)
      return(out)
    }
    else{
      stop("option must be 1, 2 or 3")
      }
  }
  # test under double or pgw model
  if(mod == "dw" | mod == "pgw"){
    if(length(credregions) != 4){
      stop("Argument credregions must be a vector of length 4.")
    }
    shape1credregion = credregions[1:2]
    shape2credregion = credregions[3:4]
    
    shape1ropehdi_res = hdi_plus_rope(nullregion = nullregion, credregion = shape1credregion)
    shape2ropehdi_res = hdi_plus_rope(nullregion = nullregion, credregion = shape2credregion)
    res = c(shape1ropehdi_res, shape2ropehdi_res)
    
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
  }

}




## END OF DOC