// weibull time-to-event model - data chunk

  int<lower = 1> N_status_e;         // number of events in sample
  int<lower = 0> N_status_c;         // number of censored observations in sample
  vector<lower = 0>[N_status_e] te;  // time observations for events
  vector<lower = 0>[N_status_c] tc;  // time observations for censored cases
  real<lower = 0> t_expect;          // meta parameter "expectation" for scale parameter theta
  real<lower = 0> t_stdev;           // meta parameter "standard dev." for scale parameter theta
  real<lower = 0> n_expect;          // meta parameter "expectation" for shape parameter nu
  real<lower = 0> n_stdev;           // meta parameter "standard dev." for shape parameter nu
