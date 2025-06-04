#' Fit model to time-to-event data (non-Bayesian)
#' 
#' Fits a time-to-event model using the specified distribution via maximum
#' likelihood estimation.
#' 
#' @param dat A data frame or matrix with time information in first column and 
#' event information (binary status) in second column.
#' @param tte.dist A character string specifying the distribution to use for the
#' model. Options are "w" for Weibull, "dw" for double Weibull, and
#' "pgw" for power generalized Weibull.
#' @param censor time of censoring
#' 
#' @return Output of the fitted model. For Weibull models, a \code{summary.survreg} object;
#' in case of \code{tte.dist = "dw"} a list of two \code{summary.survreg} objects
#' with \code{$uncens} containing the maximum likelihood estimate
#' obtained from the model to the tte data as is, and \code{$cens} containing the 
#' maximum likelihood estimate
#' obtained from the model to the tte data censored at mid of observation period; 
#' for the pgw model, a list containing the maximum likelihood estimate 
#' (log of all parameters) obtained via `nlm`.
#' 
#' @export
#' 


fwsp_model <- function(dat, 
                             tte.dist = c("w", "dw", "pgw"),
                             censor = 365
                             ) {
  tte.dist = match.arg(tte.dist, c("w", "dw", "pgw"))
  
  if(tte.dist == "w"){
    # fit model
    res.w = summary(survival::survreg(survival::Surv(time = dat[,1], event = dat[,2])~1, dist = "weibull"))
    return(res.w)
  }
  
  if(tte.dist == "dw"){
    # extract data censored a middle of observation period
    dat.c = dat
    dat.c[dat$time > censor/2,1] = ceiling(censor/2)
    dat.c[dat$time > censor/2,2] = 0
    # fit models
    res.w = summary(survival::survreg(survival::Surv(time = dat[,1], event = dat[,2])~1, dist = "weibull"))
    res.c.w = summary(survival::survreg(survival::Surv(time = dat.c[,1], event = dat.c[,2])~1, dist = "weibull"))
    return(list(uncens = res.w, cens = res.c.w))
  }
  
  if(tte.dist == "pgw"){
    # fit model
    res.pgw = try(nlm(mllk_pgw, p = c(0,0,0), dat = dat, hessian = T))
    return(res.pgw)
  }
  
  return(mod)
}
