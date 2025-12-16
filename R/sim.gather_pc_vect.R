#' Extract parameter vector from pc_list
#'
#' Inner function to extract one simulation scenario's parameter from one 
#' data generating process specification and one fitting parameter specification.
#'
#'
#' @param dgp_pars_vect  vector containing 6 elements, namely \enumerate{
#'       \item the sample size N,
#'       \item the background rate br,
#'       \item the ADR rate adr,
#'       \item the ADR mean time quantile percentage,
#'       \item the ADR relative standard deviation from the mean time,
#'       \item the study end time point.
#'       }
#' @param fit_pars_vect vector containing 3 elements, namely \enumerate{
#'       \item the time-to-event distribution (either \code{"w", "dw"} or \code{"pgw"}),
#'       \item the prior distribution (either \code{"fl", "ll", "fg"} or \code{"gg"}),
#'       \item prior belief (one of the characters specified in \link{sim.setup_simpars}).
#'       }
#' 
#' @noRd

sim.gather_pc_vect = function(dgp_pars_vect, fit_pars_vect){
  pc_vect = data.frame(dgp_pars_vect, fit_pars_vect)
  return(pc_vect)
}
