// log survival function (= complementary cumulative distr. function) for pgW distribution
  real pgW_lccdf(real t, real theta, real nu, real gamma){
  return 1 - (1 + (t/theta)^nu)^(1/gamma);
  }
