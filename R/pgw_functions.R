#' The power generalized Weibull distribution
#' 
#' @description Survival, hazard, cumulative distribution,
#' density, quantile and sampling function for the power generalized 
#' Weibull (PgW) distribution with parameters \code{scale}, \code{shape} and \code{powershape}.
#' 
#' @param x vector of quantiles
#' @param p vector of probabilities
#' @param n number of observations
#' 
#' @param scale scale parameter
#' @param shape shape parameter
#' @param powershape power shape parameter
#' 
#' @param log FALSE (default); if TRUE, the logarithm of the survival probability is returned
#' 
#'
#' @return A vector of cumulative probability values evaluated at each of the inserted quantiles.
#' 
#' @details The survival function of the PgW distribution is:
#' \deqn{
#'     S(x) = \exp \left\{ 1 - \left[ 1 + \left(\frac{x}{\theta}\right)^{\nu}\right]^{\frac{1}{\gamma}} \right\}.
#' }
#' The hazard function is
#' \deqn{
#'\frac{\nu}{\gamma\theta^{\nu}}\cdot x^{\nu-1}\cdot  \left[ 1 + \left(\frac{x}{\theta}\right)^{\nu}\right]^{\frac{1}{\gamma-1}}
#' }
#' The cumulative distribution function is then \eqn{F(x) = 1 - S(x)} and the density function 
#' is \eqn{S(x)\cdot h(x)}. The quantile function is the inverse of the cumulative 
#' distribution function \eqn{F^{-1}(x)}.
#'
#' If both shape parameters equal 1, the PgW distribution reduces to the exponential distribution
#' (see \code{\link[stats]{dexp}}) with \eqn{\texttt{rate} = 1/\texttt{scale}}
#' If the power shape parameter equals 1, the PgW distribution simplifies to the Weibull distribution
#' (see \code{\link[stats]{dweibull}}) with the same parametrization.
#' 
#' If parameter values are not specified, they are set as
#' \code{scale = 1, shape = 1, powershape = 1} per default.
#' 
#' @references
#' \insertRef{nikulin2016}{BWSPsignal}
#'
#' @name pgw
#'
NULL



#' @rdname pgw
#' 
#' @export
#'
## Survival function

spgw = function(x, scale = 1, shape = 1, powershape = 1, log = FALSE){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape
  
  log_survival_values = 1 - (1 + (x/theta)^nu)^(1/gamma)
  if(log == TRUE){
    return(log_survival_values)
  }
  else{
    survival_values = exp(log_survival_values)
    return(survival_values)
  }
  
}


#' @rdname pgw
#' 
#' @export
#'

## hazard function

hpgw = function(x, scale = 1, shape = 1, powershape = 1, log = FALSE){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape
  
  log_hazard_values = log(nu) - log(gamma) -nu*log(theta) + (nu-1)*log(x) + (1/gamma - 1)*log(1 + (x/theta)^nu)
  if(log == TRUE){
    return(log_hazard_values)
  }
  else{
    hazard_values = exp(log_hazard_values)
    return(hazard_values)
  }
  
}



#' @rdname pgw
#' 
#' @export
#'

## cumulative distribution function

ppgw = function(x, scale = 1, shape = 1, powershape = 1){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape

  log_surv_values = 1 - (1 + (x/theta)^nu)^(1/gamma)
  cdf_values = 1 - exp(log_surv_values)

  return(cdf_values)

}



#' @rdname pgw
#' 
#' @export
#'

## density function

dpgw = function(x, scale = 1, shape = 1, powershape = 1, log = FALSE){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape
  
  log_pdf_values = log(nu) - log(gamma) -nu*log(theta) + (nu-1)*log(x) + (1/gamma - 1)*log(1 + (x/theta)^nu) +1 - (1 + (x/theta)^nu)^(1/gamma)
  if(log == TRUE){
    return(log_pdf_values)
  }
  else{
    pdf_values = exp(log_pdf_values)
    return(pdf_values)
  }
  
}



#' @rdname pgw
#' 
#' @export
#'

## quantile function

qpgw = function(p, scale = 1, shape = 1, powershape = 1){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape
  
  
  # inverse cumulative distribution fct.
  inv_ppgw = theta*( ( 1 - log(1-p) )^gamma - 1 )^(1/nu)
  
  
  return(inv_ppgw)
  
}

#' @rdname pgw
#' 
#' @export
#'

## random sample generator

rpgw = function(n, scale = 1, shape = 1, powershape = 1){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape
  
  u = stats::runif(n) # generate random sample from uniform distribution
  
  # inverse cumulative distribution fct.
  inv_ppgw = theta*( ( 1 - log(1-u) )^gamma - 1 )^(1/nu)
  
  
  return(inv_ppgw)
  
}

