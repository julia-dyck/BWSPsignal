#' Prepare Bayesian pgW fitting for simulation study
#' 
#' 
#' Formatting of simulated data generated with \code{\link{datagen_tte}} as 
#' building block for tuning the Bayesian WSP test via simulation study.
#'
#' @param ttedat time-to-event data set
#' @param pc vector representing one parameter combination used in simulation study
#'
#' @return A list containing all information to be inserted as \code{datstan} 
#' argument into the \code{\link{fit_pgw_tte}} function.
#'
#' @export
#' 


## CONCRETE SPECIFICATION DEPENDS ON tte.dist and prior.belief 

sim.fit.prep = function(ttedat, pc, pc_list){
  # ttedat = data set in time-event-format generated from pc
  # pc contains N, br, adr.rate, adr.when, adr.relsd, censor, tte.dist, prior.dist, prior.belief
  # pc_list contains all additional parameters necessary to specify the simulation study 
  # (eg. prior means and sds depending on prior belief, 
  # additional parameters constant for all simulations)
  
  ### NEW ---------------------------------
  # how many and which prior beliefs
  prior.beliefs = pc_list$input$prior.belief
  return(prior.beliefs) ## HIER WEITER
  
  if(pc$tte.dist == "w"){
    if(pc$prior.belief == "none"){  ## HOW MANY BELIEVES AND WHICH NAMES DEPENDS ON INPUT IN sim.setup_simpars fct...
      datstan = tte2priordat_w(ttedat = ttedat, 
                               scale.mean = 1, scale.sd = 10,
                               shape.mean = 1, shape.sd = 10,
                               powershape.mean = 1, powershape.sd = 10)
    }
    datstan = tte2priordat_w(ttedat = ttedat, 
                             scale.mean = 
                             )
  }
  
  
  ### OLD ---------------------------------
  adr.assumption = pc[8]

  # 1. prior starting values reflecting hyp: "no adr risk over time"
  # data reformatting
  if(adr.assumption == "none"){
    datstan = tte2standat(dat = ttedat,
                                scale.mean = 1, scale.sd = 10,
                                shape.mean = 1, shape.sd = 10,
                                powershape.mean = 1, powershape.sd = 10)
  }

  # 2. prior starting values reflecting hyp: "adr occuring at beginning of observation period"
  # data reformatting
  if(adr.assumption == "beginning"){
    datstan = tte2standat(dat = ttedat,
                                      scale.mean = 1, scale.sd = 10,
                                      shape.mean = 0.207, shape.sd = 10,
                                      powershape.mean = 1, powershape.sd = 10)
  }

  # 3. prior starting values reflecting hyp: "adr occuring towards end of observation period"
  # data reformatting
  if(adr.assumption == "end"){
    datstan = tte2standat(dat = ttedat,
                                      scale.mean = 300, scale.sd = 10,
                                      shape.mean = 4, shape.sd = 10,
                                      powershape.mean = 1, powershape.sd = 10)
  }

  # 4. prior starting values reflecting hyp: "adr occuring within middle of the observation period"
  # data reformatting
  if(adr.assumption == "middle"){
    datstan = tte2standat(dat = ttedat,
                                      scale.mean = 20, scale.sd = 10,
                                      shape.mean = 5.5, shape.sd = 10,
                                      powershape.mean = 14, powershape.sd = 10)
  }
  return(datstan)
}
