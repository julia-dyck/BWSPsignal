#' Prepare Bayesian pgW fitting
#'
#' Formatting of simulated data generated with \code{link{datagen_tte}} as building
#' block for tuning the Bayesian WSP test via simulation study.
#'
#'
#' @param ttedat time-to-event data set
#' @param pc vector representing one parameter combination used in simulation study
#'
#' @return list containing all information to be inserted as \code{datstan} 
#' argument into the \code{\link{fit_pgw_tte} function.
#'
#'
#' @export
#'



fit.prep = function(ttedat, pc){
  # ttedat = data set in time-event-format generated from pc
  # pc contains N, br, adr.rate, adr.when, adr.relsd, censor, dist.ass, adr.ass
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
