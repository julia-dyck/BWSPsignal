#'
#'
#'
#'@export

logprior_repar = function(m_t, s_t){
  # same reparametrization as in stan
  # given random variable t~logN with expectation m_t and variance s_t^2,
  # location mu (or meanlog in plnorm fct) and scale (or sdlog in plnorm fct)
  # are decucted as
  mu = log(m_t) - log(s_t^2/m_t^2 -1)*0.5
  sigma = sqrt(log(s_t^2/m_t^2 +1))
  return(c(mu, sigma)) # returned parametrization
  # can be used to calculate ROPEs with qlnorm
}
