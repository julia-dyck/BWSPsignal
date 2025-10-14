#' Minus-log-Likelihood for pgW distribution
#' 
#' Minus-log-likelihood function of the power generalized Weibull distribution 
#' for time-to-event data.
#' To be used with `nlm` or similar optimization functions for maximum likelihood estimation.
#' 
#' @param par vector of pgw parameters (scale, shape, powershape)
#' @param dat data frame containing tte data
#' 
#' 
#' @return minus-log-likelihood value given the parameters and the data
#' 



mllk_pgw = function(par, dat){
  theta = exp(par[1])
  nu = exp(par[2])
  gamma = exp(par[3])
  dens.survi = function(x){
    ((nu/gamma)*(x[1]^(nu-1)/theta^nu)*(1+(x[1]/theta)^nu)^(1/gamma -1)*exp(1 - (1 + (x[1]/theta)^nu)^(1/gamma)))^x[2]*(exp(1 - (1 + (x[1]/theta)^nu)^(1/gamma)))^(1-x[2])
  }
  -sum(log(apply(dat,1,dens.survi)))
}

## test mlogl
#M = matrix(c(10,5,1,1), ncol = 2)
#M
#mllk_pgw(c(0,0,0),M)

