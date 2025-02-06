#' Reparametrization of Lognormal mean and sd
#' 
#' 
#' `logprior_repar` can be used after specifying a Lognormal prior in terms of
#' mean `m_t` and standard deviation `s_t` to calculate the location `meanlog` 
#' and scale `sdlog` as defined and used in \code{\link[stats]{dlnorm}}.
#' 
#' @param m_t mean of the Lognormal distribution
#' @param s_t standard deviation of the Lognormal distribution
#' 
#' @return a vector with the location `meanlog` and scale `sdlog` of the 
#' Lognormal distribution
#' 
#' @details
#' Expectation (mean) and standard deviation of a Lognormal random variable \eqn{X} 
#' parametrized in terms of the location `meanlog` \eqn{\mu} and scale `sdlog`
#' \eqn{\sigma} are given by
#' 
#' \deqn{E(X) = \exp\left( \mu + \frac{1}{2\sigma^2}  \right)}
#' \deqn{sd(X) = \sqrt{\left(\exp\left( 2\mu + \sigma^2 \right) \left( \exp(\sigma^2) - 1 \right) \right)}.}
#' 
#' If \eqn{|sd(X)|>|E(X)|>0} rearraging above equations to
#' 
#' \deqn{\mu = \log(E(X)) - \frac{1}{2} \log\left(\frac{sd(X)^2}{E(X)^2} - 1\right)}
#' 
#' \deqn{\sigma = \sqrt{\log\left(\frac{sd(X)^2}{E(X)^2} + 1\right)}.}
#' 
#' is possible such that the location parameter `meanlog` or \eqn{\mu} and the 
#' scale parameter `logmean` or \eqn{\sigma} of the Lognormal distribution are 
#' obtained.
#' 
#' @examples
#' # obtain location and scale for Lognormal distribution with mean = 1 and sd = 10
#' m = 1; s = 10
#' lnorm_pars = logprior_repar(m_t = m, s_t = s)
#' mu = lnorm_pars[1] # location parameter
#' sigma = lnorm_pars[2] # scale parameter
#' 
#' # test: sample from the Lognormal distribution with the obtained parameters
#' lnorm_sample = rlnorm(10000000, meanlog = mu, sdlog = sigma)
#' m_emp = mean(lnorm_sample); m_emp # estimated mean
#' s_emp = sd(lnorm_sample); s_emp # estimated sd
#'
#'@export

logprior_repar = function(m_t, s_t){
  if(m_t <= 0 | s_t <= 0) stop("m_t and s_t must be positive.")
  if(s_t <= m_t) stop("s_t must be larger than the m_t.")
  
  # same reparametrization as in stan
  # given random variable t~logN with expectation m_t and variance s_t^2,
  # location mu (or meanlog in plnorm fct) and scale (or sdlog in plnorm fct)
  # are decucted as
  mu = log(m_t) - log(s_t^2/m_t^2 -1)*0.5
  sigma = sqrt(log(s_t^2/m_t^2 +1))
  return(c(mu, sigma)) # returned parametrization
  # can be used to calculate ROPEs with qlnorm
}
