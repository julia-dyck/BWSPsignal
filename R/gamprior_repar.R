#' Reparametrization of Gamma mean and sd
#' 
#' 
#' `gamprior_repar` can be used after specifying a Gamma prior in terms of
#' mean `m_t` and standard deviation `s_t` to calculate the `shape` 
#' and `rate` as defined and used in \code{\link[stats]{dgamma}}.
#' 
#' @param m_t mean of the Gamma distribution
#' @param s_t standard deviation of the Gamma distribution
#' 
#' @return a vector with the `shape` 
#' and `rate` of the Gamma distribution
#' 
#' @details
#' Expectation (mean) and standard deviation of a Gamma distributed random variable \eqn{X} 
#' parametrized in terms of the shape parameter \eqn{\alpha}
#' and rate parameter \eqn{\beta} are given by
#' 
#' \deqn{E(X) = \frac{\alpha}{\beta}}
#' \deqn{sd(X) = \frac{\alpha}{\beta^2}}
#' 
#' For \eqn{E(X), \;sd(X)>0} rearraging above equations to
#' 
#' \deqn{\alpha = \frac{\mu^2}{\sigma^2}}
#' 
#' \deqn{\beta = \frac{\mu}{\sigma^2}}
#' 
#' is possible such that the shape parameter \eqn{\alpha} 
#' and rate parameter \eqn{\beta} of the Gamma distribution are 
#' obtained.
#' 
#' @examples
#' # obtain shape and rate for Gamma distribution with mean = 1 and sd = 10
#' m = 1; s = 10
#' gam_pars = gamprior_repar(m_t = m, s_t = s)
#' shape = gam_pars[1] # shape parameter
#' rate = gam_pars[2] # rate parameter
#' 
#' # test: sample from the Gamma distribution with the obtained parameters
#' gamma_sample = rgamma(10000000, shape = shape, rate = rate)
#' m_emp = mean(gamma_sample); m_emp # estimated mean
#' s_emp = sd(gamma_sample); s_emp # estimated sd
#'
#'@export

gamprior_repar = function(m_t, s_t){
  if(m_t <= 0 | s_t <= 0) stop("m_t and s_t must be positive.")
  
  # same reparametrization as in stan
  # given random variable t~Gamma with expectation m_t and variance s_t^2,
  # shape (also called shape in pgamma fct) and rate (also called rate in pgamma fct)
  # are decucted as
  shape = m_t^2 / s_t^2
  rate = m_t / s_t^2
  return(c(shape, rate)) # returned parameterization
  # can be used to calculate ROPEs with qgamma
}
