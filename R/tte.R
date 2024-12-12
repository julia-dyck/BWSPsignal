#' test data for development of simstudy code
#'
#' Simulated data generated with \code{datagen_tte(c(100, 0.1, 1, 0.5, 0.05, 365))} 
#' with parameters N = 100, br = 0.1, adr.rate = 1, adr.when = 0.5, adr.relsd = 0.05, censor = 365.
#' 
#' @format  A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{time}{event-time if an event was observed, or censoring time if no event was observed,}
#'   \item{status}{event status; 1 if an event was observed, 0 if no event was observed,}
#'}
#' 
#' @seealso [datagen_tte()]
#'
"tte"