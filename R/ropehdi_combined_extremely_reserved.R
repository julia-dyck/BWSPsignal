#' Combined ropehdi test (for nu and gamma in one fct)
#'
#' @details
#' Combination mechanism ("intuitive" version) reflects the thought:
#'
#' If H0 is rejected for both shape parameters nu and gamma of the power generalized
#' Weibull distribution, then the combined test rejects H0.
#'
#' All other combinations of single test results indicate no signal.
#'
#'
#' @export

ropehdi_combined_extra_reserved = function(credregion.vect, nullregion){

  nu.credregion = credregion.vect[1:2]
  ga.credregion = credregion.vect[3:4]

  nu.ropehdi_res = ropehdi(nullregion = nullregion, credregion = nu.credregion)
  ga.ropehdi_res = ropehdi(nullregion = nullregion, credregion = ga.credregion)
  res = c(nu.ropehdi_res, ga.ropehdi_res)

  if(sum(is.na(res)) == 2){ # both undecided
    return(0)
  }

  if(sum(is.na(res)) == 1){ # one undecided
    return(0)
  }

  if(sum(is.na(res)) == 0){ # both decided
    out = ifelse(sum(res) == 2, 1, 0)
    return(out)
  }
}
