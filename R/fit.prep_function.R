#' Prepare Bayesian pgW fitting
#'
#' Contains data preparation for stanmodel.
#'
#'
#' @param survdat time-event-sample
#' @param pc one parameter combination used in simulation study
#'
#' @return a list containing all information to be inserted into fit.fgg, fit.fll,
#'         fit.ggg, or fit.lll function.
#'
#'
#' @export
#'



fit.prep = function(survdat, pc){
  # survdat = data set in time-event-format generated from pc
  # pc contains N, br, adr.rate, adr.when, adr.relsd, censor, dist.ass, adr.ass
  adr.assumption = pc[8]

  # 1. prior starting values reflecting hyp: "no adr risk over time"
  # data reformatting
  if(adr.assumption == "none"){
    datstan = survdat2pgwstanmodeldat(dat = survdat,
                                      scale.mean = 1, scale.sd = 10,
                                      shape.mean = 1, shape.sd = 10,
                                      powershape.mean = 1, powershape.sd = 10)
  }

  # 2. prior starting values reflecting hyp: "adr occuring at beginning of observation period"
  # data reformatting
  if(adr.assumption == "beginning"){
    datstan = survdat2pgwstanmodeldat(dat = survdat,
                                      scale.mean = 1, scale.sd = 10,
                                      shape.mean = 0.207, shape.sd = 10,
                                      powershape.mean = 1, powershape.sd = 10)
  }

  # 3. prior starting values reflecting hyp: "adr occuring towards end of observation period"
  # data reformatting
  if(adr.assumption == "end"){
    datstan = survdat2pgwstanmodeldat(dat = survdat,
                                      scale.mean = 300, scale.sd = 10,
                                      shape.mean = 4, shape.sd = 10,
                                      powershape.mean = 1, powershape.sd = 10)
  }

  # 4. prior starting values reflecting hyp: "adr occuring within middle of the observation period"
  # data reformatting
  if(adr.assumption == "middle"){
    datstan = survdat2pgwstanmodeldat(dat = survdat,
                                      scale.mean = 20, scale.sd = 10,
                                      shape.mean = 5.5, shape.sd = 10,
                                      powershape.mean = 14, powershape.sd = 10)
  }
  return(datstan)
}
