// fixed ie not estimated scale parameter
// estimated shape parameter (ass. to be gamma distributed)
// estimated powershape parameter (ass. to be gamma distributed)


functions {
  // log probability density function for power generalized Weibull (pgW) distribution
  #include /functions/pgw_log_density_function.stan

  // log survival function (= complementary cumulative distr. function) for pgW distribution
  #include /functions/pgw_log_survival_function.stan

  // reparametrization for gamma distribution:
  #include /functions/gamma_repar_function.stan

}

data {
  #include /data/pgw_tte_data.stan
}
parameters {
  #include /parameters/pgw_scalefixed_parameters.stan
}
model {
  real n_alpha = gamma_expect_stdev_to_alpha(n_expect, n_stdev); // transform parametrization to predefined gamma parameterization
  real n_beta = gamma_expect_stdev_to_beta(n_expect, n_stdev);   // ----"----
  real g_alpha = gamma_expect_stdev_to_alpha(g_expect, g_stdev); // ----"----
  real g_beta = gamma_expect_stdev_to_beta(g_expect, g_stdev);   // ----"----

  target += gamma_lpdf(nu | n_alpha, n_beta);
  target += gamma_lpdf(gamma | g_alpha, g_beta);
  for(i in 1:N_status_e){
    target += pgw_lpdf(te[i] | t_expect, nu, gamma);
  }
  for(j in 1:N_status_c){
    target += pgW_lccdf(tc[j] | t_expect, nu, gamma);
  }
}
