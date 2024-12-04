// reparametrization for lognormal distribution:

  // expected value and standard dev. (input) to -> location parameter mu
  real lognormal_expect_stdev_to_mu(real expect, real stdev){
  return log(expect) - log(stdev^2/expect^2 + 1)/2;
  }


  // expected value and standard dev. (input) to -> scale parameter sigma
  real lognormal_expect_stdev_to_sigma(real expect, real stdev){
  return sqrt(log(stdev^2/expect^2 + 1));
  }
