//
// fixed ie not estimated scale parameter
// estimated shape parameter (ass. to be lognormal distributed)
// estimated powershape parameter (ass. to be lognormal distributed)



functions {
  // log probability density function for power generalized Weibull (pgW) distribution
  #include /functions/pgw_log_density_function.stan

  // log survival function (= complementary cumulative distr. function) for pgW distribution
  #include /functions/pgw_log_survival_function.stan

  // reparametrization for lognormal distribution:
  #include /functions/lognormal_repar_function.stan

}

data {
  #include /data/pgw_tte_data.stan
}
parameters {
  #include /parameters/pgw_scalefixed_parameters.stan
}
model {
  real n_mu = lognormal_expect_stdev_to_mu(n_expect, n_stdev);       // transform parametrization to predefined lognormal parameterization
  real n_sigma = lognormal_expect_stdev_to_sigma(n_expect, n_stdev); // ----"----
  real g_mu = lognormal_expect_stdev_to_mu(g_expect, g_stdev);       // ----"----
  real g_sigma = lognormal_expect_stdev_to_sigma(g_expect, g_stdev); // ----"----

  target += lognormal_lpdf(nu | n_mu, n_sigma);
  target += lognormal_lpdf(gamma | g_mu, g_sigma);
  for(i in 1:N_status_e){
    target += pgw_lpdf(te[i] | t_expect, nu, gamma);
  }
  for(j in 1:N_status_c){
    target += pgW_lccdf(tc[j] | t_expect, nu, gamma);
  }
}
