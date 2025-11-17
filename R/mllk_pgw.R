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



mllk_pgw = function(par, dat) {
  theta  <- exp(par[1])
  nu     <- exp(par[2])
  gamma  <- exp(par[3])
  
  x <- dat[,1]
  d <- dat[,2]
  
  logdens <- dpgw(x, scale = theta, shape = nu, powershape = gamma, log = TRUE)
  logsurv <- spgw(x, scale = theta, shape = nu, powershape = gamma, log = TRUE)
  
  # log-likelihood
  ll <- d * logdens + (1 - d) * logsurv
  
  return(-sum(ll))
}



