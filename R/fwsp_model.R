#' Fit frequentist model to time-to-event data
#' 
#' Fits a frequentist model to time-to-event (tte) data via maximum
#' likelihood (ML) estimation.
#' 
#' @param dat data frame or matrix with time information in first column and 
#' event information (binary status) in second column
#' @param tte.dist character specifying the distribution for the
#' model; options are \code{"w", "dw", "pgw"} (see details)
#' @param censor time of censoring; only required for \code{"dw"}.
#' 
#' @return Output of the fitted model. For \code{"w"}, a \code{summary.survreg} object;
#' for \code{"dw"}, a list of two \code{summary.survreg} objects
#' with \code{$uncens} containing the ML estimate
#' obtained from the model to the tte data as is, and \code{$cens} containing the 
#' ML estimate obtained from the model fitted to the tte data censored at mid of observation period; 
#' for \code{pgw}, a list containing the maximum likelihood estimate 
#' (log of all parameters) obtained from \code{\link[stats]{nlm}}.
#' 
#' @details The model can be a Weibull \code{("w")},
#' a double Weibull (estimating two Weibull models - one to the data as is and 
#' one to the data censored at mid of observation period), 
#' or a power generalized Weibull \code{("pgw")} model.
#' 
#' The likelihood used in ML estimation is 
#' \deqn{\mathcal{L}(t) = \prod_{i=1}^N S(t_i)^{1-d_i}\cdot f(t_i)^{d_i}} 
#' with \eqn{S(t)} the survival function of the chosen distribution and \eqn{f(t)} the
#' density function \insertCite{nikulin2016}{BWSPsignal}. The pair \eqn{(t_i, d_i)} are the tte observations.
#' 
#' @references
#' \insertAllCited{}
#' 
#' @examples
#' head(tte)
#' fwsp_model(tte, tte.dist = "w") # Weibull model
#' fwsp_model(tte, tte.dist = "dw", censor = 365) # double Weibull model
#' fwsp_model(tte, tte.dist = "pgw") # power generalized Weibull model
#' 
#' 
#' @export
#' 


fwsp_model = function(dat, 
                      tte.dist = c("w", "dw", "pgw"),
                      censor = NULL
                      ) {
  tte.dist <- match.arg(tte.dist)
  
  # enforce censor only for dw
  if (tte.dist == "dw") {
    if (is.null(censor)) stop("Argument 'censor' must be provided when tte.dist='dw'")
  } else {
    if (!is.null(censor)) warning("'censor' is ignored when tte.dist='dw' or 'pgw'.")
  }
  
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
