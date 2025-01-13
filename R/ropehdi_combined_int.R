#' ROPE + HDI test
#' 
#' @description blabla
#'
#' @param nullregion a vector of length two denoting the lower and upper boundary of the null values defined region of practical equivalence (ROPE)
#' @param scale a vector with two entries denoting the lower and upper boundary of the posterior highest density region
#'
#' @return 0 if H_0 is accepted, 1 if H_0 is rejected and NA if no decision
#'
#' @details The decision rule is:
#'
#' - If the HDI falls completely inside the ROPE, then accept the null value for practical purposes.
#'
#' - If the HDI falls completely outside the ROPE, then reject the null value for practical purposes.
#'
#' - Else, withhold a decision.
#'
#'
#' For more information see for example
#'
#' J. K. Kruschke. Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2):270–280, 2018.
#'
#' J. K. Kruschke. Doing Bayesian data analysis: a tutorial with R, JAGS, and Stan. Academic Press, 2015.
#'
#'
#'
#' @details
#' Combination mechanism ("intuitive" version) reflects the thought:
#'
#' If H0 is accepted for both shape parameters nu and gamma of the power
#' generalized Weibull distribution, then the combined test result is
#' acceptance of H0. This is equivalent to "no adr signal".
#'
#' If H0 is rejected or there is undecidedness, then the combined test result
#' is rejection of H0.
#' @name ropehdi
#' 
NULL



#' @rdname ropehdi
#' 
#' @export
#'


bwsp_test = function(credregions, nullregion, option = c(1,2,3), mod = "pgw"){
  # later: add mod arguments c("pgW", "W", "dW") for posteriors obtained with
  # either the pgw, the Weibull or the double Weibull modelling approach (see 
  # Sauzet 2024 for that)
  nu.credregion = credregions[1:2]
  ga.credregion = credregions[3:4]

  nu.ropehdi_res = ropehdi(nullregion = nullregion, credregion = nu.credregion)
  ga.ropehdi_res = ropehdi(nullregion = nullregion, credregion = ga.credregion)
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