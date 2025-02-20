#' Prior and data setup for Bayesian Power generalized Weibull model fitting
#'
#' @description Setup of prior specifications and reformated time-to-event
#' data set as a list suitable for model fitting with \code{\link{fit_tte_pgw}}.
#' 
#' @param dat Matrix or data frame with time in the first column and event status in the second column.
#' @param scale.mean The a priori expected mean of the scale parameter.
#' @param scale.sd The priori expected standard deviation of the scale parameter.
#' @param shape.mean The priori expected mean of the shape parameter.
#' @param shape.sd The priori expected standard deviation of the shape parameter.
#' @param powershape.mean The priori expected mean of the power shape parameter.
#' @param powershape.sd The priori expected standard deviation of the power shape parameter.
#' 
#' @details The function converts time-to-event data and prior specifications for location ('.mean') and 
#' precision ('.sd') of the power generalized Weibull parameters 
#' to a suitable format for `rstan` model fitting
#' which is performed within \code{\link{fit_tte_pgw}}.
#' 
#' Prior means suitable to reflect the prior belief can be found by plotting the
#' hazard and estimating the expected event time under different parameter combinations
#' using \code{\link{plot_pgw}} or \url{https://janoleko.shinyapps.io/pgwd/}.
#' 
#' Prior standard deviations should reflect the uncertainty about the prior belief
#' (i.e. set smaller standard deviation in case of high certainty about prior belief vs. larger 
#' standard deviation in case of low certainty).
#' 
#' @export

tte2priordat_pgw = function(dat, 
                            scale.mean,
                            scale.sd,
                            shape.mean,
                            shape.sd,
                            powershape.mean,
                            powershape.sd){
  
  standat = list(N_status_e = sum(dat[,2]),
                 N_status_c = dim(dat)[1]-sum(dat[,2]),
                 te = dat[which(dat[,2]== 1),1],
                 tc = dat[which(dat[,2]== 0),1],
                 t_expect = scale.mean,
                 t_stdev = scale.sd,
                 n_expect = shape.mean,
                 n_stdev = shape.sd,
                 g_expect = powershape.mean,
                 g_stdev = powershape.sd
  )
  return(standat)
}

