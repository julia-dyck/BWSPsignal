#' Setup simulation scenarios
#' 
#' The function returns the set of simulation scenarios determined by the data generating process (dgp) parameter
#' values of interest.
#' 
#' @param N A scalar or vector of sample sizes.
#' @param br A scalar or vector of background rates.
#' @param adr.rate A scalar or vector of adverse drug reaction rates.
#' @param adr.when A scalar or vector of expected event times (relative number, e.g. 0.5 matches half of study period).
#' @param adr.when.name A vector of description/short name of expected event times (ie. name of simulated truth)
#' @param adr.relsd A scalar or vector of relative standard deviations from the adverse drug reaction times.
#' @param study.period A scalar specifying the length of the study period.
#' 
#' @return A data frame with the simulation scenarios that will be inserted into the 
#' \code{\link{sim.datagen_tte}} function.
#' 
#' @export



#### setup data generating process parameters


sim.setup_dgp_pars = function(N,           # dgp parameters
                              br,
                              adr.rate,
                              adr.when,
                              adr.relsd,
                              study.period 
){
  if(any(adr.rate == 0)){ # separate control and adr>0 cases
    adr.rate = adr.rate[-(which(adr.rate==0))]
  }
  if(any(adr.when == 0)){ # separate control and adr.when>0 cases
    adr.when = adr.when[-(which(adr.when==0))]
  }
  
  else{ # its fine, but notify user that control case is necessary
    warning("Control case (adr.rate = 0) necessary for simulation study. Value 0 is added to specified vector adr.rate.")
  }
  
  # par combis with adr.rate > 0
  pc_with_adr = expand.grid(        
    study.period = study.period,
    adr.relsd = adr.relsd,
    adr.when = adr.when, # remove 0 from adr.when,
    adr.rate = adr.rate, # remove 0 from adr.rate,         
    br = br,
    N = N
  )
  
  # par combis with adr.rate = 0 (control), adr.when then also set to 0
  
  pc_no_adr = expand.grid(       
    study.period = study.period,
    adr.relsd = NA,
    adr.when = NA,
    adr.rate = 0,
    br = br,
    N = N
  )
  
  dgp_pc = rbind(pc_no_adr,
                 pc_with_adr)
  
  
  dgp_pc = dgp_pc[, ncol(dgp_pc):1]
  dgp_pc = dgp_pc[order(dgp_pc$N),]
  rownames(dgp_pc) <- 1:nrow(dgp_pc)
  return(dgp_pc)
}

# dgp_pc = sim.setup_dgp_pars(N = c(500, 3000, 5000),
#                             br = 0.1,
#                             adr.rate = c(0.5, 1),
#                             adr.when = c(0.25, 0.5, 0.75),
#                             adr.relsd = c(0.05),
#                             study.period = 365)
# 
# dgp_pc



## END OF DOC

