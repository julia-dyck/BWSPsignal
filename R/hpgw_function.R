#' Power generalized Weibull hazard function
#'
#'
#'
#' @param x vector of quantiles
#' @param scale Scale parameter of the distribution (\eqn{\theta} in formula)
#' @param shape Shape parameter (\eqn{\nu} in formula)
#' @param powershape Powershape parameter (\eqn{\gamma} in formula)
#' @param log locigal argument; if TRUE, the logarithmized hazard value is returned
#'
#' @return A vector of hazard values evaluated at each of the inserted quantiles
#'
#'
#' The hazard function of the power generalized distribution is (Nikulin and Wu, 2016, p. 15-16):
#'
#' \deqn{
#'     h(x) = \frac{\nu}{\gamma \theta^\nu}\cdot x^{\nu-1} \cdot \left[1 + \left(\frac{x}{\theta}\right)^{\nu}\right]^{\frac{1}{\gamma} - 1}
#' }
#'
#' If both shape parameters equal 1, the pgw distribution simplifies to the exponential distribution
#' (implemented in \code{d/p/q/rexp} in Rpackage 'stats') with \eqn{rate = 1/scale}
#'
#' If the powershape parameter equals 1, the pgw distribution simplifies to the Weibull distribution
#' (implemented in \code{d/p/q/rweibull} in Rpackage 'stats') with the same parametrization.
#'
#'
#' @export
#'


## density function

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

## testing

# x= 1:10
#
# hpgw(x, scale = 2)
# hpgw(x, scale = 1, shape = 1/2)




