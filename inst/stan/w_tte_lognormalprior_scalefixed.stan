// Weibull tte model
//
// fixed ie not estimated scale parameter
// estimated shape parameter (ass. to be lognormal distributed)



functions {
  // reparametrization for lognormal distribution:
  #include /functions/lognormal_repar_function.stan
}

data {
  #include /data/w_tte_data.stan
}
parameters {
  #include /parameters/w_scalefixed_parameters.stan
}
model {
  real n_mu = lognormal_expect_stdev_to_mu(n_expect, n_stdev);       // transform parametrization to predefined lognormal parameterization
  real n_sigma = lognormal_expect_stdev_to_sigma(n_expect, n_stdev); // ----"----

  target += lognormal_lpdf(nu | n_mu, n_sigma);
  for(i in 1:N_status_e){
    target += weibull_lpdf(te[i] | t_expect, nu);
  }
  for(j in 1:N_status_c){
    target += weibull_lccdf(tc[j] | t_expect, nu);
  }
}