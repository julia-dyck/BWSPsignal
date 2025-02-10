// Weibull tte model

// estimated scale parameter (ass. to be lognormal distributed)
// estimated shape parameter (ass. to be lognormal distributed)


functions {
  // reparametrization for lognormal distribution:
  #include /functions/lognormal_repar_function.stan

}

data {
  #include /data/w_tte_data.stan //ADJUST->erstelle w_tte_data.stan & save in data folder
  }
parameters {
  #include /parameters/w_parameters.stan
}
model {
  real t_mu = lognormal_expect_stdev_to_mu(t_expect, t_stdev);       // transform parametrization to predefined lognormal parameterization
  real t_sigma = lognormal_expect_stdev_to_sigma(t_expect, t_stdev); // ----"----
  real n_mu = lognormal_expect_stdev_to_mu(n_expect, n_stdev);       // ----"----
  real n_sigma = lognormal_expect_stdev_to_sigma(n_expect, n_stdev); // ----"----

  target += lognormal_lpdf(theta | t_mu, t_sigma);
  target += lognormal_lpdf(nu | n_mu, n_sigma);
  for(i in 1:N_status_e){
    target += weibull_lpdf(te[i] | theta, nu);
  }
  for(j in 1:N_status_c){
    target += weibull_lccdf(tc[j] | theta, nu);
  }
}
