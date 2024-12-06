#' extract meta and prior statistics from stanfit object 2.0
#'
#' The function extracts information about the model fitting and prior specifications for the parameters to be estimated.
#'
#' @param stanfit.object the estimated stan model output
#' @param stan.dat the data inserted for model fitting
#'
#'
#' @return Information about the model fitting, such as:
#' \item{run.min}{running time in minutes}
#' \item{th.pr.mean}{prior mean of parameter theta}
#' \item{th.pr.sd}{prior standard deviation of parameter theta}
#' \item{nu.pr.mean}{prior mean of parameter nu}
#' \item{nu.pr.sd}{prior standard deviation of parameter nu}
#' \item{ga.pr.mean}{prior mean of parameter gamma}
#' \item{ga.pr.sd}{prior standard deviation of parameter gamma}
#'
#' @export
#'

stanfit.to.fitstats = function(stanfit.object, stan.dat){

  obj = stanfit.object

  # prior specifications
  th.pr.mean = stan.dat$t_expect
  th.pr.sd = stan.dat$t_stdev
  nu.pr.mean = stan.dat$n_expect
  nu.pr.sd = stan.dat$n_stdev
  ga.pr.mean = stan.dat$g_expect
  ga.pr.sd = stan.dat$g_stdev

  run.min = sum(rstan::get_elapsed_time(obj))/60 # in minutes

  fitstats = data.frame(run.min,
                        th.pr.mean,
                        th.pr.sd,
                        nu.pr.mean,
                        nu.pr.sd,
                        ga.pr.mean,
                        ga.pr.sd
                        )
  return(fitstats)
}

