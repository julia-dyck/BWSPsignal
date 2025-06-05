#' Frequentist Shape Parameter Test
#' 
#' Frequentist hypothesis test based on the shape parameter(s) of a distribution of the 
#' Weibull family
#' 
#
#' 
#' @param mod.output estimation output resulting from \code{\link{fwsp_model}}
#' @param tte.dist character indicating the modelling approach; options are
#' \code{"w", "dw", "pgw"}
#' @param credlevel vector of credibility levels for the tests to be performed 
#' 
#' @return A vector containing the test results for each credibility level.
#' 
#' @details This function tests the null hypothesis that the shape parameter(s) of the
#' Weibull family distribution are equal to one.
#' 
#' The distribution specific definitions of the null hypothesis can be found in 
#' \insertCite{sauzet2024;textual}{BWSPsignal} in Table 1.
#' 
#' 
#'
#' @references 
#' \insertAllCited{}
#'
#' @export
#' 


fwsp_test = function(mod.output, tte.dist = c("w", "dw", "pgw"), credlevel = 1 - c(1:10/1000, 2:10/100)){
  
  tte.dist = match.arg(tte.dist, c("w", "dw", "pgw"))
  
  if(tte.dist == "w"){
    testres = fwsp_test_w(mod.output, credlevel)
  }
  
  if(tte.dist == "dw"){
    testres = fwsp_test_dw(mod.output, credlevel)
  }
  
  if(tte.dist == "pgw"){
    testres = fwsp_test_pgw(mod.output, credlevel)
  }
  
  # name each vector entry according to the credibility level
  names(testres) = paste0("fwsp_", tte.dist, "_", credlevel)
  return(testres)
}
