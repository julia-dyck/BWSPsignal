#' Power generalized Weibull probability density function
#'
#'
#'
#' @param x vector of quantiles
#' @param scale Scale parameter of the distribution (\eqn{\theta} in formula)
#' @param shape Shape parameter (\eqn{\nu} in formula)
#' @param powershape Powershape parameter (\eqn{\gamma} in formula)
#' @param log locigal argument; if TRUE, the logarithmized density value is returned
#'
#' @return A vector of density values evaluated at each of the inserted quantiles
#'
#'
#' The density function of the power generalized distribution is (Nikulin and Wu, 2016, p. 15-16):
#'
#' \deqn{
#'     f(x) = \frac{\nu}{\gamma \theta^\nu}\cdot x^{\nu-1} \cdot \left[1 + \left(\frac{x}{\theta}\right)^{\nu}\right]^{\frac{1}{\gamma} - 1} \cdot exp \left\{ 1 - \left[ 1 + \left(\frac{x}{\theta}\right)^{\nu}\right]^{\frac{1}{\gamma}} \right\}
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




## testing

# x = 1:3
# dpgw(x, scale = 0.5, shape = 1, powershape = 1)
# dexp(x, rate = 2)


# x = 1:3
# dpgw(x, scale = 0.5, shape = 3, powershape = 1)
# dweibull(x, shape = 3, scale = 0.5)

