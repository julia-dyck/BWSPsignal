### to be worked into run.sim

#### load packages
library(survival)   # for Weibull time-event modelling
library(dplyr)      # for data manipulation
library(tidyverse)  # for data manipulation
library(ROCR)       # for AUC calculation
library(stringr)    # for table generation and regular expression
library(xtable)     # for table generation



################################
#### log-Likelihood for pgW ####
################################


# Input
#  ## par = Parameters for the pgW 
#         = log(theta) (scale parameter), 
#           log(nu) (1st shape parameter), 
#           log(gamma) (2nd shape parameter)
#     -> theta, nu, gamma >0, therefore transformation via exp()
#  ## dat = 1st column contains timepoints, 2nd column contains status

# further explanation
#  ## x = time and status of one individual

# output
# ## value of the minus-loglikelihood value

mlogl = function(par, dat){
  theta = exp(par[1])
  nu = exp(par[2])
  gamma = exp(par[3])
  dens.survi = function(x){
    ((nu/gamma)*(x[1]^(nu-1)/theta^nu)*(1+(x[1]/theta)^nu)^(1/gamma -1)*exp(1 - (1 + (x[1]/theta)^nu)^(1/gamma)))^x[2]*(exp(1 - (1 + (x[1]/theta)^nu)^(1/gamma)))^(1-x[2])
  }
  -sum(log(apply(dat,1,dens.survi)))
}

## test mlogl
#M = matrix(c(10,5,1,1), ncol = 2)
#M
#mlogl(c(0,0,0),M)


###############################################
#### pgW test function for multiple alphas ####
###############################################

# function performs a test on both shape parameters of the power generalized Weibull distribution
# Each test contains a test for nu and a test for gamma
# and is performed for different significance levels, in our simulation for
# alphas = 0.001, 0.002,...0.009, 0.01, 0.02,...,0.1

# Input: eo = estimation output 
#        alphas = vector of significance levels eg. c(1:10/1000, 2:10/100)

# output: vector of length length(alphas) containing the test results for each alpha 


test.pgW = function(eo, alphas = c(1:10/1000, 2:10/100)){ # REWRITE TO DEPEND ON CREDLEVEL
  
  if(is.vector(eo) == F){
    rej.H0 = rep(NA, length(alphas))
  }
  else if(is.numeric(eo$hessian) == F){
    rej.H0 = rep(NA, length(alphas))
  }
  else{
    varmatrix = try(solve(eo$hessian))
    print(varmatrix)
    if(is.matrix(varmatrix) == F){  
      rej.H0 = rep(NA, length(alphas))
    }
    else if(sum(is.na(varmatrix)) > 0){ 
      rej.H0 = rep(NA, length(alphas))
    }
    else if(sum(is.nan(varmatrix)) > 0){ 
      rej.H0 = rep(NA, length(alphas))
    }
    else if(varmatrix[2,2] < 0 | varmatrix[3,3] < 0){
      rej.H0 = rep(NA, length(alphas))
    }
    else{
      test.pgW.inside = function(alpha){
        CI.nu = eo$estimate[1] + c(-1,1)*qnorm(1-alpha/2)*sqrt(varmatrix[2,2])
        rej.nu = as.numeric(0 <= CI.nu[1] | CI.nu[2] <= 0) #here: 1 if rejected, 0 if not rejected
        CI.gamma = eo$estimate[2] + c(-1,1)*qnorm(1-alpha/2)*sqrt(varmatrix[3,3])
        rej.gamma = as.numeric(0 <= CI.gamma[1] | CI.gamma[2] <= 0)
        return(c(rej.nu, rej.gamma))
      }
      rej = sapply(alphas, test.pgW.inside) 
      rej.nu = rej[1,]
      rej.gamma = rej[2,]
      rej.H0 = rej.nu*rej.gamma #here: 1 if H0 rejected, 0 if H0 not rejected 
      ## ADD DIFFERENT SENSITIVITY LEVELS HERE AS WELL?
    }
    return(pgWtest = rej.H0)
  }
}



## REWRITE THE FOLLOWING INSIDE sim.fit.to.1.fct i think, st. after data generating, the bayesian and the frequentist model is fit and frequ. test results are saved.

#############################################################
#### function to generate data & perform the basic tests ####
#############################################################

# input:
# ## parameter combination (n, br, adr, sd.adr, censor)

# output: 
# table called data.basic containing the results of the
# ## Weibull-Test wrt. the original data set (column 1:19)
# ## Weibulltest wrt. the data set artificially cencored after half the observation time (column 20:38)
# ## Power-generalized-Weibull-Test (column 39:57)

# each test is performed 19 times for varying significance level 
# c(0.001, 0.002, 0.003,..., 0.009,
#   0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10)

# 1 is interpreted as "signal" indicating an ADR effect,
# 0 is interpreted as "no signal", meaning, that no ADR effect is found

library(survival)

gen.to.basictest = function(par){
  # preparation
  options(warn=-1)
  dat = datagen(par)
  dat.c = dat
  censor = par[5]
  dat.c[dat$time > censor/2,1] = ceiling(censor/2)
  dat.c[dat$time > censor/2,2] = 0
  
  # estimate on original data assuming pgW distribution
  start.pgW = c(0,0,0) # fix starting value (for log-likelihood maximisation)
  res.pgW = try(nlm(logl,start.pgW,dat,hessian=T))
  
  # estimate on original data assuming Weibull distribution
  res.W = summary(survreg(Surv(time = dat$time, event = dat$status)~1, dist = "weibull"))
  
  # estimate on censored data assuming Weibull distribution
  res.c.W = summary(survreg(Surv(time = dat.c$time, event = dat.c$status)~1, dist = "weibull"))
  
  # pgW test
  rej.pgW = test.pgW(res.pgW)
  
  # Weibull-test
  rej.W = test.W(res.W)
  
  # Weibull-test on the cencored data
  rej.c.W = test.W(res.c.W)
  
  return(c(W = rej.W, c.W = rej.c.W, pgW = rej.pgW))
}

##### testing
# par.combis[12,]
# test.res = gen.to.basictest(par.combis[1,])
# test.res





