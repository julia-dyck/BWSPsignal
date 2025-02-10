// Weibull tte model

// fixed ie not estimated scale parameter
// estimated shape parameter (ass. to be gamma distributed)


functions {
  // reparametrization for lognormal distribution:
  #include /functions/gamma_repar_function.stan

}

data {
  #include /data/w_tte_data.stan 
  }
parameters {
  #include /parameters/pgw_parameters.stan
}
model {
  real n_alpha = gamma_expect_stdev_to_alpha(n_expect, n_stdev); // transform parametrization to predefined gamma parameterization
  real n_beta = gamma_expect_stdev_to_beta(n_expect, n_stdev);   // ----"----

  target += gamma_lpdf(nu | n_alpha, n_beta);
  for(i in 1:N_status_e){
    target += weibull_lpdf(te[i] | t_expect, nu);
  }
  for(j in 1:N_status_c){
    target += weibull_lccdf(tc[j] | t_expect, nu);
  }
}