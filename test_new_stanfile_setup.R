

library(bADRfcts)

#prep the data
head(testdat)
standat = survdat2pgwstanmodeldat(dat = testdat,
                                  scale.mean = 1, scale.sd = 10,
                                  shape.mean = 1, shape.sd = 10,
                                  powershape.mean = 1, powershape.sd = 10)

# fit the model:
testmod = fit.lll(datstan = standat, cores = 1)
testmod

output = rstan::stan(
    file = "stanfiles/pgw_tte_gammaprior.stan",  # Stan program
    model_name = "pgw_tte_lognormalprior_model", # model name
    data = standat,   # named list of data
    chains = 1,       # number of Markov chains
    warmup = 100,    # number of warmup iterations per chain
    iter = 1100,     # total number of iterations per chain (including warmup)
    cores = 1   # number of cores (one per chain)
  )
output
