#' Visualization of the power generalized Weibull distribution
#'
#'
#' @param scale Scale parameter of the distribution (\eqn{\theta} in formula)
#' @param shape Shape parameter (\eqn{\nu} in formula)
#' @param powershape Powershape parameter (\eqn{\gamma} in formula)
#' @param ... optional plot arguments (expept \code{type, lwd, main})
#'
#' @return A plot of the PDF, CDF, hazard and survival function
#' 
#' @examples
#' plotpgw(scale = 2, shape = 5, powershape = 10)
#'
#' @export
#'


plotpgw = function(scale = 1, shape = 1, powershape = 1,...){

  sim = rpgw(1000, scale, shape, powershape)
  m = mean(sim)
  med = median(sim)
  std = sd(sim)

  x_upper = m + 2*std
  if(is.finite(x_upper)){
    x = seq(from = 0, to = x_upper, by = x_upper/100)
  }
  else{
    x = seq(from = 0, to = 10000, by = 0.1)
  }

  values = list()
  values[[1]] = cbind(x, dpgw(x, scale, shape, powershape))
  values[[2]] = cbind(x, spgw(x, scale, shape, powershape))
  values[[3]] = cbind(x, ppgw(x, scale, shape, powershape))
  values[[4]] = cbind(x, hpgw(x, scale, shape, powershape))

  function_name = c("density fct.", "survival fct.", "cum. dist. fct.", "hazard fct.")
  par_name = c("scale", "shape", "powershape")
  par_vect = c(scale, shape, powershape)

  par(mfrow = c(2,2))
  for(i in 1:4){
    plot(values[[i]],
         type = "l",
         lwd = 3,
         main = function_name[i],
         ...
         )
    if(i == 1){
      abline(v = m, lwd = 3, lty = 2, col = "darkgrey")
      legend("topright",
             legend = "emp. mean",
             lty = 2,
             col = "darkgrey"
             )
      }
    if(i == 2){
      legend("topright",
             legend = paste0(par_name, " = ", par_vect)
               )
      }
  }
  par(mfrow = c(1,1))

 cat("empirical mean = ", m)
 cat("\n")
 cat("empirical median = ", med)
}


# plotpgw(scale = 2, shape = 5, powershape = 10, col = 4)

