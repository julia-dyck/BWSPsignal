#' Power generalized Weibull cumulative distribution function
#'
#'
#'
#' @param x vector of quantiles
#' @param scale Scale parameter of the distribution (\eqn{\theta} in formula)
#' @param shape Shape parameter (\eqn{\nu} in formula)
#' @param powershape Powershape parameter (\eqn{\gamma} in formula)
#'
#' @return A vector of cumulative probability values evaluated at each of the inserted quantiles
#'
#'
#' The density function of the power generalized distribution is (Nikulin and Wu, 2016, p. 15-16):
#'
#' \deqn{
#'     F(x) = 1 - exp \left\{ 1 - \left[ 1 + \left(\frac{x}{\theta}\right)^{\nu}\right]^{\frac{1}{\gamma}} \right\}
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





## cumulative distr. fct. function

ppgw = function(x, scale = 1, shape = 1, powershape = 1){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape

  log_surv_values = 1 - (1 + (x/theta)^nu)^(1/gamma)
  cdf_values = 1 - exp(log_surv_values)

  return(cdf_values)

}




