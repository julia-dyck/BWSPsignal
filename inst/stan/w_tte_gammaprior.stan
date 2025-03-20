// Weibull tte model

// estimated scale parameter (ass. to be gamma distributed)
// estimated shape parameter (ass. to be gamma distributed)


functions {
  // reparametrization for gamma distribution:
  #include /functions/gamma_repar_function.stan
}

data {
  #include /data/w_tte_data.stan
}
parameters {
  #include /parameters/w_parameters.stan
}
model {
  real t_alpha = gamma_expect_stdev_to_alpha(t_expect, t_stdev); // transform parameterization to predefined gamma parameterization
  real t_beta = gamma_expect_stdev_to_beta(t_expect, t_stdev);   // ----"----
  real n_alpha = gamma_expect_stdev_to_alpha(n_expect, n_stdev); // ----"----
  real n_beta = gamma_expect_stdev_to_beta(n_expect, n_stdev);   // ----"----
  
  target += gamma_lpdf(theta | t_alpha, t_beta);
  target += gamma_lpdf(nu | n_alpha, n_beta);
  for(i in 1:N_status_e){
    target += weibull_lpdf(te[i] | theta, nu);
  }
  for(j in 1:N_status_c){
    target += weibull_lccdf(tc[j] | theta, nu);
  }
}

