#'setup of test parameters
#'
#'

sim.setup_test_pars = function(post.ci.type = c("ETI", "HDI"),
                               cred.level = seq(0.5, 0.95, by = 0.05),
                               sensitivity.option = 1:3){
  
  # message("The rope reflecting the null-hypothesis of constant hazard is will be an equal-tailed confidence interval (ETI) based on shape parameters' prior distribution, mean, and sd specified in function sim.setup_fit_pars().")
  
  expand.grid(post.ci.type = post.ci.type,
              cred.level = cred.level,
              sensitivity.option = sensitivity.option)
}

# test_pc = sim.setup_test_pars()

