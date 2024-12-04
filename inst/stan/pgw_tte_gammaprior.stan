// estimated scale parameter (ass. to be gamma distributed)
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
  #include /parameters/pgw_parameters.stan
}
model {
  real t_alpha = gamma_expect_stdev_to_alpha(t_expect, t_stdev); // transform parameterization to predefined gamma parameterization
  real t_beta = gamma_expect_stdev_to_beta(t_expect, t_stdev);   // ----"----
  real n_alpha = gamma_expect_stdev_to_alpha(n_expect, n_stdev); // ----"----
  real n_beta = gamma_expect_stdev_to_beta(n_expect, n_stdev);   // ----"----
  real g_alpha = gamma_expect_stdev_to_alpha(g_expect, g_stdev); // ----"----
  real g_beta = gamma_expect_stdev_to_beta(g_expect, g_stdev);   // ----"----

  target += gamma_lpdf(nu | n_alpha, n_beta);
  target += gamma_lpdf(gamma | g_alpha, g_beta);
  for(i in 1:N_status_e){
    target += pgw_lpdf(te[i] | theta, nu, gamma);
  }
  for(j in 1:N_status_c){
    target += pgW_lccdf(tc[j] | theta, nu, gamma);
  }
}

