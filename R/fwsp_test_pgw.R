#' pgW test function for multiple alphas
#' 
#' This function tests the null hypothesis that the logarithm of the shape parameters 
#' of the power generalized Weibull distribution are equal to zero based on the 
#' shape estimates and their estimated standard errors extracted from the estimated
#' hessian.
#' 
#' @param eo estimation outpuf resulting from nlm to maximize the log-likelihood 
#' of the power generalized Weibull distribution (`mllk_pgw`)
#' @param alphas significance levels for the tests to be performed (= 1 - credibility level)
#' 
#' @return A vector of length `length(alphas)` containing the test results for each alpha.


fwsp_test_pgW = function(eo, alphas = c(1:10/1000, 2:10/100)){ # REWRITE TO DEPEND ON CREDLEVEL
  
  if(is.vector(eo) == F){
    rej.H0 = rep(NA, length(alphas))
  }
  else if(is.numeric(eo$hessian) == F){
    rej.H0 = rep(NA, length(alphas))
  }
  else{
    varmatrix = try(solve(eo$hessian))
    print(varmatrix)
    if(is.matrix(varmatrix) == F){  
      rej.H0 = rep(NA, length(alphas))
    }
    else if(sum(is.na(varmatrix)) > 0){ 
      rej.H0 = rep(NA, length(alphas))
    }
    else if(sum(is.nan(varmatrix)) > 0){ 
      rej.H0 = rep(NA, length(alphas))
    }
    else if(varmatrix[2,2] < 0 | varmatrix[3,3] < 0){
      rej.H0 = rep(NA, length(alphas))
    }
    else{
      test.pgW.inside = function(alpha){
        CI.nu = eo$estimate[1] + c(-1,1)*qnorm(1-alpha/2)*sqrt(varmatrix[2,2])
        rej.nu = as.numeric(0 <= CI.nu[1] | CI.nu[2] <= 0) #here: 1 if rejected, 0 if not rejected
        CI.gamma = eo$estimate[2] + c(-1,1)*qnorm(1-alpha/2)*sqrt(varmatrix[3,3])
        rej.gamma = as.numeric(0 <= CI.gamma[1] | CI.gamma[2] <= 0)
        return(c(rej.nu, rej.gamma))
      }
      rej = sapply(alphas, test.pgW.inside) 
      rej.nu = rej[1,]
      rej.gamma = rej[2,]
      rej.H0 = rej.nu*rej.gamma #here: 1 if H0 rejected, 0 if H0 not rejected 
    }
    return(pgWtest = rej.H0)
  }
}

