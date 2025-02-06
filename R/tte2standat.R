#' reformat time-to-event data for usage in `fit_pgw_tte()`
#'
#'
#'
#' @description Reformatting of a matrix or data frame containing a time-to-event
#' data set with time in the first column and the event status in the second column.
#' 
#' @param dat Matrix or data frame with time in the first column and event status in the second column.
#' @param scale.mean The a priori expected mean of the scale parameter.
#' @param scale.sd The priori expected standard deviation of the scale parameter.
#' @param shape.mean The priori expected mean of the shape parameter.
#' @param shape.sd The priori expected standard deviation of the shape parameter.
#' @param powershape.mean The priori expected mean of the power shape parameter.
#' @param powershape.sd The priori expected standard deviation of the power shape parameter.
#'
#' @details Converts time-event data to a suitable format for `rstan` model fitting.
#'
#'
#'@export



tte2standat = function(dat,
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

# # testing
#
# testd = datagenpgw(c(100, 32, scale = 3, shape = 4, powershape = 8))
# head(testd)
#
# testdstanmod = tte2standat(testd,
#                            scale.mean = 3,
#                            scale.sd = 4,
#                            shape.mean = 5,
#                            shape.sd = 6,
#                            powershape.mean = 7,
#                            powershape.sd = 8)
