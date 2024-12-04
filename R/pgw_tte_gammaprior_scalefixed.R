#' Bayesian time-to-event model with PGW likelihood and fix-gam-gam prior
#'
#' @export
#'
#' @param tte_datstan A list containing data for the model; can be created using
#' \code{\link{survdat2pgwstanmodeldat}}
#' @param ... Additional arguments to pass to \code{\link{rstan::sampling}}
#'
#' @return An object of class `stanfit` returned by \code{\link{rstan::sampling}}
#'
#' @details
#' Additional details...
#'


pgw_fgg <- function(tte_datstan,...){
  out <- rstan::sampling(stanmodels$pgw_tte_gammaprior_scalefixed,
                         data = tte_datstan,
                         ...
                         )
  return(out)
}
