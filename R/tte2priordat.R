#' Prior and data setup for Bayesian survival model fitting
#'
#' @description
#' Prepare time-to-event data and prior specifications for model fitting
#' with \code{\link{bwsp_model}}. 
#'
#' @param dat matrix or data frame with time in the first column and event status 
#' in the second column
#' @param tte.dist character indicating the modelling approach; options are \code{"w"}, 
#' \code{"dw"}, \code{"pgw"}
#' @param scale.mean prior mean of the scale parameter
#' @param scale.sd prior standard deviation (sd) of the scale parameter
#' @param shape.mean prior mean of the shape parameter
#' @param shape.sd prior sd of the shape parameter
#' @param scale_c.mean prior mean of the scale parameter for censored-at-half data 
#' (only for \code{tte.dist="dw"})
#' @param scale_c.sd prior sd of the scale parameter for censored-at-half data 
#' (only for \code{tte.dist="dw"})
#' @param shape_c.mean prior mean of the shape parameter for censored-at-half data 
#' (only for \code{tte.dist="dw"})
#' @param shape_c.sd prior sd of the shape parameter for censored-at-half data 
#' (only for \code{tte.dist="dw"})
#' @param powershape.mean prior mean of the power shape parameter (only for 
#' \code{tte.dist="pgw"})
#' @param powershape.sd prior sd of the power shape parameter (only for 
#' \code{tte.dist="pgw"})
#' 
#'
#' @details
#' Only the parameters relevant to the chosen \code{tte.dist} must be provided, that is:
#' \itemize{
#'   \item for \code{"w"}: \code{scale.mean}, \code{scale.sd}, \code{shape.mean}, \code{shape.sd}
#'   \item for \code{"dw"}: \code{scale.mean}, \code{scale.sd}, \code{shape.mean}, \code{shape.sd},
#'   \code{scale_c.mean}, \code{scale_c.sd}, \code{shape_c.mean}, \code{shape_c.sd}
#'   \item for \code{"pgw"}: \code{scale.mean}, \code{scale.sd}, \code{shape.mean}, \code{shape.sd}, 
#'   \code{powershape.mean}, \code{powershape.sd}
#' }
#' 
#' Prior means suitable to reflect the prior belief can be worked out by plotting the
#' hazard and estimating the expected event time under different parameter combinations
#' using \code{\link{plot_pgw}}(\code{powershape = 1} reduces the power 
#' generalized Weibull distribution to Weibull) or \url{https://janoleko.shinyapps.io/pgwd/}.
#' 
#' Prior standard deviations should reflect the uncertainty about the prior belief
#' (i.e. set smaller standard deviation in case of high certainty about prior belief vs. larger 
#' standard deviation in case of low certainty).
#'
#' @return A named list in the format expected by \code{\link{bwsp_model}}.
#'
#' @examples
#' 
#' tte2priordat(dat = tte, tte.dist = "w", 
#'              scale.mean = 10, scale.sd = 2, 
#'              shape.mean = 1.5, shape.sd = 15)
#'              
#' tte2priordat(dat = tte, tte.dist = "dw", 
#'              scale.mean = 10, scale.sd = 2, 
#'              shape.mean = 1.5, shape.sd = 15,
#'              scale_c.mean = 5, scale_c.sd = 1, 
#'              shape_c.mean = 1, shape_c.sd = 10)
#'              
#' tte2priordat(dat = tte, tte.dist = "pgw", 
#'              scale.mean = 10, scale.sd = 2, 
#'              shape.mean = 1.5, shape.sd = 15,
#'              powershape.mean = 3, powershape.sd = 20)
#' 
#'
#' @export
#' 

tte2priordat = function(dat,
                        tte.dist = c("w","dw","pgw"),
                        scale.mean, scale.sd,
                        shape.mean, shape.sd,
                        scale_c.mean = NULL, scale_c.sd = NULL,
                        shape_c.mean = NULL, shape_c.sd = NULL,
                        powershape.mean = NULL, powershape.sd = NULL) {
  
  tte.dist <- match.arg(tte.dist)
  
  args_list <- as.list(environment())  # capture all arguments in a list
  
  # Helper: check required arguments
  req <- function(...) {
    dots_expr <- as.list(substitute(list(...)))[-1]
    dots_vals <- list(...)
    miss <- sapply(dots_vals, is.null)
    if (any(miss)) {
      stop(
        sprintf(
          "For tte.dist='%s' additional arguments must be provided: %s",
          tte.dist,
          paste(sapply(dots_expr[miss], deparse), collapse = ", ")
        )
      )
    }
  }
  
  # Warn about irrelevant arguments
  warn_irrelevant <- function(args_list, forbidden_args) {
    provided <- forbidden_args[sapply(forbidden_args, function(x) !is.null(args_list[[x]]))]
    if (length(provided)) {
      warning(sprintf(
        "For tte.dist='%s' the following arguments are ignored: %s",
        tte.dist,
        paste(provided, collapse = ", ")
      ))
    }
  }
  
  if (tte.dist == "w") {
    warn_irrelevant(args_list, c("scale_c.mean","scale_c.sd","shape_c.mean","shape_c.sd",
                                 "powershape.mean","powershape.sd"))
    req(scale.mean, scale.sd, shape.mean, shape.sd)
    return(tte2priordat_w(dat, scale.mean, scale.sd, shape.mean, shape.sd))
  }
  
  if (tte.dist == "dw") {
    warn_irrelevant(args_list, c("powershape.mean","powershape.sd"))
    req(scale.mean, scale.sd, shape.mean, shape.sd,
        scale_c.mean, scale_c.sd, shape_c.mean, shape_c.sd)
    return(tte2priordat_dw(dat, scale.mean, scale.sd, shape.mean, shape.sd,
                           scale_c.mean, scale_c.sd, shape_c.mean, shape_c.sd))
  }
  
  if (tte.dist == "pgw") {
    warn_irrelevant(args_list, c("scale_c.mean","scale_c.sd","shape_c.mean","shape_c.sd"))
    req(scale.mean, scale.sd, shape.mean, shape.sd,
        powershape.mean, powershape.sd)
    return(tte2priordat_pgw(dat, scale.mean, scale.sd, shape.mean, shape.sd,
                            powershape.mean, powershape.sd))
  }
}
