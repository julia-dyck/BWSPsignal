#' Prior and data setup for Bayesian double Weibull model fitting
#'
#' @description Setup of prior specifications and reformated time-to-event
#' data set as a list suitable for model fitting with \code{\link{fit_dw_tte}}.
#' 
#' @param dat Matrix or data frame with time in the first column and event status in the second column.
#' @param scale.mean The a priori expected mean of the scale parameter.
#' @param scale.sd The priori expected standard deviation of the scale parameter.
#' @param shape.mean The priori expected mean of the shape parameter.
#' @param shape.sd The priori expected standard deviation of the shape parameter.
#' @param scale_c.mean The a priori expected mean of the scale parameter for the
#' data censored at half of the observation period.
#' @param scale_c.sd The priori expected standard deviation of the scale parameter
#' for the data censored at half of the observation period.
#' @param shape_c.mean The priori expected mean of the shape parameter for the
#' data censored at half of the observation period.
#' @param shape_c.sd The priori expected standard deviation of the shape parameter
#' for the data censored at half of the observation period.
#' 
#' @details The function converts time-to-event data and prior specifications for location ('.mean') and 
#' precision ('.sd') of the double Weibull parameters 
#' to a suitable format for `rstan` model fitting
#' which is performed within \code{\link{fit_dw_tte}}.
#' 
#' Prior means suitable to reflect the prior belief can be found by plotting the
#' hazard and estimating the expected event time under different parameter combinations
#' using \code{\link{plot_pgw}} with 'powershape = 1' (reducing the pgw distribution 
#' to the Weibull distribution) or \url{https://janoleko.shinyapps.io/pgwd/}.
#' 
#' Prior standard deviations should reflect the uncertainty about the prior belief
#' (i.e. set smaller standard deviation in case of high certainty about prior belief vs. larger 
#' standard deviation in case of low certainty).
#' 


tte2priordat_dw = function(dat,
                          scale.mean,
                          scale.sd,
                          shape.mean,
                          shape.sd,
                          scale_c.mean,
                          scale_c.sd,
                          shape_c.mean,
                          shape_c.sd){
  # for normal weibull model
  standat_uncens = list(N_status_e = sum(dat[,2]),
                        N_status_c = dim(dat)[1]-sum(dat[,2]),
                        te = dat[which(dat[,2]== 1),1],
                        tc = dat[which(dat[,2]== 0),1],
                        t_expect = scale.mean,
                        t_stdev = scale.sd,
                        n_expect = shape.mean,
                        n_stdev = shape.sd
  )
  
  # censor data at half of observation period (op)
  dat_c = dat
  half_op = ceiling(max(dat[,1])/2)
  tobecensored = which(dat[,1] > half_op)
  dat_c[tobecensored, 2] = 0
  dat_c[which(dat_c[,2] == 0), 1] = half_op
  
  standat_cens = list(N_status_e = sum(dat_c[,2]),
                      N_status_c = dim(dat_c)[1]-sum(dat_c[,2]),
                      te = dat[which(dat_c[,2]== 1),1],
                      tc = dat[which(dat_c[,2]== 0),1],
                      t_expect = scale_c.mean,
                      t_stdev = scale_c.sd,
                      n_expect = shape_c.mean,
                      n_stdev = shape_c.sd
  )
  standat = list(uncens = standat_uncens, cens = standat_cens)
  return(standat)
}