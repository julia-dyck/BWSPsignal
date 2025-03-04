## parameters for simulation study ---------------------------


#### Parameter combinations for the data generating processes (dgps)

# The data dgp of interest should reflect the characteristics of the data to be 
# investigated with the BWSP signal detection tool.
# 
# Have a look at `?datagen_tte` for information on how each parameter contributes 
# to the data generation and `?sim.setup_dgp_pars` for information the setup of 
# multiple dgps for the simulation study.


#### Parameter combinations for the tuning parameters

# The tuning parameters are used to specify the model fitting and test setup of 
# the BWSP signal detection tool.

# Depending on the distributional choices and prior beliefs to be specified, the 
# function returns promts to specify the prior means and standard deviations of all scale and shape parameters.

# The prior belief `none` is mandatory and reflects the constant-hazard assumption 
# formalized as "all shape parameter means = 1".


pc_list = sim.setup_sim_pars(N = c(500, 3000, 5000),       # dgp parameters
                        br = 0.1,
                        adr.rate = c(0.5, 1),
                        adr.when = c(0.25, 0.5, 0.75),
                        adr.relsd = 0.05,
                        study.period = 365,
                        
                        tte.dist = c("w", "dw", "pgw"),          # tuning parameters
                        prior.belief = c("none", "beginning", "middle", "end"),
                        prior.dist = c("fgg", "ggg", "fll", "lll"),
                        post.ci.type = c("ETI", "HDI"),
                        cred.level = seq(0.5, 0.95, by = 0.05),
                        sensitivity.option = 1:3,
                        
                        reps = 100,
                        resultpath = paste0(getwd(), "/results_raw")
                        )


save(pc_list, file = "pc_list.RData")

## END OF DOC