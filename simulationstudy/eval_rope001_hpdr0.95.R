### ROPE test 001 (= first try)
### init 2023-08-01
### applied to simstudy01 merged_results
### rope001 = 95% CI of lognormal prior distribution under H0 (mean = 1, sd = 10)
### phdr001 = 95% HPDR of empirical posterior distribution


#-------------------------------------------------------------------------------
# load result table (later merged results table)

testres = bADRfcts::merge_results(pc_table = pc[1,])
dim(testres)
View(testres)

#-------------------------------------------------------------------------------
# define ROPE:
logprior_repar = function(m_t, s_t){
  # same reparametrization as in stan
  # given random variable t~logN with expectation m_t and variance s_t^2,
  # location mu (or meanlog in plnorm fct) and scale (or sdlog in plnorm fct)
  # are decucted as
  mu = log(m_t) - log(s_t^2/m_t^2 -1)*0.5
  sigma = sqrt(log(s_t^2/m_t^2 +1))
  return(c(mu, sigma)) # returned parametrization
  # can be used to calculate ROPEs with qlnorm
}

# H0 mean and sd reparametrization:
# for lognormal distributed prior:
logpars = logprior_repar(m_t = 1,s_t = 10)

rope001 = qlnorm(c(0.025,0.975), meanlog =logpars[1], sdlog = logpars[2])
rope001

# extract HPDR (highest posterior density region)
nu.hdi0.95 = cbind(testres$nu.hdi0.95l,testres$nu.hdi0.95u)
ga.hdi0.95 = cbind(testres$ga.hdi0.95l,testres$ga.hdi0.95u)

# TODO: apply ROPE test to one row

ropehdi(nullregion = rope001, credregion = nu.hdi0.95)
ropehdi(nullregion = rope001, credregion = ga.hdi0.95)
# with rule "one rejection -> overall rejection of H0"
# == accept overall H0 only if acc.nullvalue == T for both ropetests


ropehdi_combined_int = function(nullregion, nu.credregion, ga.credregion){

  nu.ropehdi_res = ropehdi(nullregion = nullregion, credregion = nu.credregion)
  ga.ropehdi_res = ropehdi(nullregion = nullregion, credregion = ga.credregion)

  if(nu.ropehdi_res == 0 && ga.ropehdi_res == 0){
    return(0) # 0 == accept H0
  }
  else{
    return(1) # 1 == reject H0
  }
}

binary_outcome = ropehdi_combined(nullregion = rope001,
                                  credregion.vect = c(nu.hdi0.95, ga.hdi0.95))

binary_outcome













