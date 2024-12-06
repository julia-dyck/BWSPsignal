#' Power generalized Weibull distribution: sample generator
#'
#'
#'
#'
#' @param n number of observations
#' @param scale Scale parameter of the distribution (\eqn{\theta} in formula)
#' @param shape Shape parameter (\eqn{\nu} in formula)
#' @param powershape Powershape parameter (\eqn{\gamma} in formula)
#'
#' @return A vector of observations generated from the power generalized Weibull distribution.
#'
#'
#' The realizations are generated with the analytical inversion method (see for example
#' \url{https://datasciencegenie.com/how-to-generate-any-random-variable/}).
#'
#'
#' @export
#'





## sample generator

rpgw = function(n, scale = 1, shape = 1, powershape = 1){
  # renaming to match the formula
  theta = scale
  nu = shape
  gamma = powershape

  u = runif(n) # generate random sample from uniform distribution

  # inverse cumulative distribution fct.
  inv_ppgw = theta*( ( 1 - log(1-u) )^gamma - 1 )^(1/nu)


  return(inv_ppgw)

}



## testing

# par(mfrow = c(1,2))
# set.seed(2071994)
# sample = rpgw(10000)
# sample
# hist(sample, prob = T)
# lines(1:150/10, dpgw(1:150/10), col = 2, lwd = 3)
#
#
# sample_exp = rexp(10000)
# hist(sample_exp, prob= T)
# lines(1:150/10, dpgw(1:150/10), col = 3, lwd = 3)
#
#
# summary(sample)
# summary(sample_exp)


