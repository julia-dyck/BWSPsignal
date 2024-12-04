// log probability density function for power generalized Weibull (pgW) distribution
  real pgw_lpdf(real t, real theta, real nu, real gamma){
  return log(nu) - log(gamma) - nu*log(theta) + (nu-1)*log(t) + (1/gamma - 1)*log(1 + (t/theta)^nu)  + 1 - (1 + (t/theta)^nu)^(1/gamma);
  }
