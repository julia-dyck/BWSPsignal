#' Power generalized Weibull distribution: quantile function
#'
#' The quantile function is the inverse function of the cumulative distribution
#' function (here implemented as \code{ppgw}).
#'
#' The formula is:
#' \deqn{
#' q(u) = F^{-1}(u) = \theta \left\{ \left[ 1 - \text{log}(1-u) \right]^{\gamma} - 1 \right\}^{\frac{1}{\nu}}  ]
#' }
#'
#'
#'
#' @param p percentile in (0,1)
#' @param scale Scale parameter of the distribution (\eqn{\theta} in formula)
#' @param shape Shape parameter (\eqn{\nu} in formula)
#' @param powershape Powershape parameter (\eqn{\gamma} in formula)
#'
#' @return corresponding quantile value
#'
#'
#'
#' @export
#'


qpgw = function(p, scale = 1, shape = 1, powershape = 1){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape


  # inverse cumulative distribution fct.
  inv_ppgw = theta*( ( 1 - log(1-p) )^gamma - 1 )^(1/nu)


  return(inv_ppgw)

}
