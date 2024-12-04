// reparameterization for gamma distribution:

  // expected value and standard dev. (input) to -> shape parameter alpha
  real gamma_expect_stdev_to_alpha(real expect, real stdev){
  return expect^2/stdev^2;
  }

  // expected value and standard dev. (input) to -> rate parameter beta
  real gamma_expect_stdev_to_beta(real expect, real stdev){
  return expect/stdev^2;
  }
