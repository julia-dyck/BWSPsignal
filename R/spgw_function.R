#' Power generalized Weibull survival function
#'
#'
#'
#' @param x vector of quantiles
#' @param scale Scale parameter of the distribution (\eqn{\theta} in formula)
#' @param shape Shape parameter (\eqn{\nu} in formula)
#' @param powershape Powershape parameter (\eqn{\gamma} in formula)
#' @param log locigal argument; if TRUE, the logarithmized survival probability is returned
#'
#' @return A vector of survival probability values evaluated at each of the inserted quantiles
#'
#'
#' The survival function of the power generalized distribution is (Nikulin and Wu, 2016, p. 15-16):
#'
#' \deqn{
#'    S(x) = exp \left\{ 1 - \left[ 1 + \left(\frac{x}{\theta}\right)^{\nu}\right]^{\frac{1}{\gamma}} \right\}
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
