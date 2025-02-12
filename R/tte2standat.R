#' reformat time-to-event data for model fitting
#'
#'
#'
#' @description Reformatting of a matrix or data frame containing a time-to-event
#' data set with time in the first column and the event status in the second column
#' to a list suitable for model fitting with \code{\link{fit_w_tte}},
#' \code{\link{fit_dw_tte}} or \code{\link{fit_pgw_tte}}.
#' 
#' @param dat Matrix or data frame with time in the first column and event status in the second column.
#' @param mod modelling approach to be used; options are "w" for Weibull or "pgw" for power generalized Weibull.
#' @param scale.mean The a priori expected mean of the scale parameter.
#' @param scale.sd The priori expected standard deviation of the scale parameter.
#' @param shape.mean The priori expected mean of the shape parameter.
#' @param shape.sd The priori expected standard deviation of the shape parameter.
#' @param powershape.mean The priori expected mean of the power shape parameter.
#' @param powershape.sd The priori expected standard deviation of the power shape parameter.
#' @param scale_c.mean The a priori expected mean of the scale parameter for the
#' data censored at mid of the observation period.
#' @param scale_c.sd The priori expected standard deviation of the scale parameter
#' for the data censored at mid of the observation period.
#' @param shape_c.mean The priori expected mean of the shape parameter for the
#' data censored at mid of the observation period.
#' @param shape_c.sd The priori expected standard deviation of the shape parameter
#' for the data censored at mid of the observation period.
#' 
#' @details Converts time-event data to a suitable format for `rstan` model fitting
#' which is performed within the \code{\link{fit_w_tte}},
#' \code{\link{fit_dw_tte}} and \code{\link{fit_pgw_tte}} functions.
#' 
#' The subset of parameters or their prior mean and standard deviation, respectively,
#' to be specified depends on the \code{mod} argument. For the Weibull model
#' (\code{mod = "w"}), the scale and shape parameters are required.
#' 
#' For the
#' double Weibull approach (\code{mod = "dw"}), the scale and shape parameters are
#' and the scale and shape parameters for the censored data are required. 
#' 
#' For the power generalized Weibull model (\code{mod = "pgw"}), the scale, shape
#' and power shape parameters are required. 
#' 
#'
#'
#'@export



tte2standat = function(dat, 
                       mod = c("w", "dw", "pgw"),
                       scale.mean = 1,
                       scale.sd = 10,
                       shape.mean = 1,
                       shape.sd = 10,
                       powershape.mean = 1,
                       powershape.sd = 10,
                       scale_c.mean = 1,
                       scale_c.sd = 10,
                       shape_c.mean = 1,
                       shape_c.sd = 10){
  if(mod == "w"){
    # check whether scale.mean, scale.sd, shape.mean, shape.sd are provided
    if(missing(scale.mean) | missing(scale.sd) | missing(shape.mean) | missing(shape.sd)){
      stop("For mod = w, scale.mean, scale.sd, shape.mean, shape.sd must be provided.")
    }
    standat = list(N_status_e = sum(dat[,2]),
                   N_status_c = dim(dat)[1]-sum(dat[,2]),
                   te = dat[which(dat[,2]== 1),1],
                   tc = dat[which(dat[,2]== 0),1],
                   t_expect = scale.mean,
                   t_stdev = scale.sd,
                   n_expect = shape.mean,
                   n_stdev = shape.sd
    )
  }
  else if(mod == "dw"){
    # check whether scale.mean, scale.sd, shape.mean, shape.sd and same with _c are provided
    if(missing(scale.mean) | missing(scale.sd) | missing(shape.mean) | missing(shape.sd) |
       missing(scale_c.mean) | missing(scale_c.sd) | missing(shape_c.mean) | missing(shape_c.sd)){
      stop("For mod = dw, scale.mean, scale.sd, shape.mean, shape.sd, scale_c.mean, scale_c.sd, shape_c.mean, shape_c.sd must be provided.")
    }
    standat_uncens = list(N_status_e = sum(dat[,2]),
                   N_status_c = dim(dat)[1]-sum(dat[,2]),
                   te = dat[which(dat[,2]== 1),1],
                   tc = dat[which(dat[,2]== 0),1],
                   t_expect = scale.mean,
                   t_stdev = scale.sd,
                   n_expect = shape.mean,
                   n_stdev = shape.sd
    )
    # censor data at mit of observation period (op)
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
  }
  else if(mod == "pgw"){
    # check whether scale.mean, scale.sd, shape.mean, shape.sd, powershape.mean, powershape.sd are provided
    if(missing(scale.mean) | missing(scale.sd) | missing(shape.mean) | missing(shape.sd) |
       missing(powershape.mean) | missing(powershape.sd)){
      stop("For mod = pgw, scale.mean, scale.sd, shape.mean, shape.sd, powershape.mean, powershape.sd must be provided.")
    }
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
  }
  else{
    stop("Argument mod must be either 'w' or 'pgw'.")
  }
  return(standat)
}


