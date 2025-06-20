#' Frequentist PgW Shape Parameter Test
#' 
#' Frequentist hypothesis test based on the shape parameters of the power 
#' generalized Weibull distribution.
#' 
#
#' 
#' @param mod.output estimation output resulting from `fit_mod_tte_freq(..., tte.dist = "pgw")`
#' @param credlevel vector of credibility levels for the tests to be performed 
#' 
#' @return A vector containing the test results for each credibility level.
#' 
#' @details This function tests the null hypothesis that the logarithm of the shape parameters 
#' of the power generalized Weibull distribution are equal to zero based on the 
#' shape estimates and their estimated standard errors extracted from the estimated
#' hessian.
#' 
#' @export


fwsp_test_pgw = function(mod.output, credlevel = 1 - c(1:10/1000, 2:10/100)){ 
  
  # check whether mod.output is a list
  if(!inherits(mod.output, "list")){
    stop("Argument mod.output must be a list returned by fit_mod_tte_freq(..., tte.dist = 'pgw').")
  }
  alphas = 1 - credlevel
  
  if(is.vector(mod.output) == F){
    rej.H0 = rep(NA, length(alphas))
  }
  else if(is.numeric(mod.output$hessian) == F){
    rej.H0 = rep(NA, length(alphas))
  }
  else{
    varmatrix = try(solve(mod.output$hessian))
    # print(varmatrix)
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
        CI.nu = mod.output$estimate[1] + c(-1,1)*qnorm(1-alpha/2)*sqrt(varmatrix[2,2])
        rej.nu = as.numeric(0 <= CI.nu[1] | CI.nu[2] <= 0) #here: 1 if rejected, 0 if not rejected
        CI.gamma = mod.output$estimate[2] + c(-1,1)*qnorm(1-alpha/2)*sqrt(varmatrix[3,3])
        rej.gamma = as.numeric(0 <= CI.gamma[1] | CI.gamma[2] <= 0)
        return(c(rej.nu, rej.gamma))
      }
      rej = sapply(alphas, test.pgW.inside) 
      rej.nu = rej[1,]
      rej.gamma = rej[2,]
      # combine
      # rule: reject H0 only, if both shapes differ significantly from null value
      rej.H0 = rej.nu*rej.gamma 
    }
    return(rej.H0) # here: 1 if H0 rejected, 0 if H0 not rejected 
  }
}





