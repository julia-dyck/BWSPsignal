#' Evaluate execution times by tte and prior distribution
#'
#' Summarizes execution times of the models fitted during the simulation study grouped
#' by time-to-event (tte) and prior distribution types to guide the
#' tte and prior distributional choices (along with \code{\link{eval.non_conv_cases}} 
#' and \code{\link{eval.eff_sample_sizes}}.
#'
#' @param pc_list list containing simulation parameters (see \code{\link{sim.setup_simpars}})
#'
#' @return list with summary statistics (`$summary`), a ggplot2 object (`$plot`), 
#' and cleaned data (`$res.ex_times.table`) which is the base for summary and plot.
#'
#' @export


eval.execution_times = function(pc_list){
 
  if (!exists("res")) { 
  # obtain res table
  tryCatch({
    load(paste0(pc_list$add$resultpath, "/res.RData"))
    message("res.RData successfully loaded")
  }, error = function(cond) {
    sim.merge_results(pc_list, save = T)
    load(paste0(pc_list$add$resultpath, "/res.RData"))
    print(" batches merged and loaded")
  })
  }
  else{
    message("Object `res` loaded in current environment is used to extract execution times.")
  }
  
  # select relevant variables
  time.df = res[, c("tte.dist", "prior.dist", "run.min")]
  # adjust format
  time.df$tte.dist <- as.factor(unlist(time.df$tte.dist))
  time.df$prior.dist <- as.factor(unlist(time.df$prior.dist))
  time.df$run.min = as.numeric(unlist(time.df$run.min)) 
  
  # execution time dist per tte.dist and prior.dist
  time.summaries = dplyr::summarise(
    dplyr::group_by(time.df, tte.dist, prior.dist),
    min = min(run.min, na.rm = TRUE),
    first_qu = quantile(run.min, 0.25, na.rm = TRUE),
    median = median(run.min, na.rm = TRUE),
    mean = mean(run.min, na.rm = TRUE),
    third_qu = quantile(run.min, 0.75, na.rm = TRUE),
    max = max(run.min, na.rm = TRUE),
    .groups = "drop"
  )
  
  
  # group variable for plot
  time.df$group <- interaction(time.df$tte.dist, time.df$prior.dist, sep = " - ")
  
  
  # plot
  p = ggplot2::ggplot(time.df, ggplot2::aes(x = group, y = run.min)) +
    ggplot2::geom_boxplot(width = 0.5, fill = "lightgrey") +
    ggplot2::labs(
      x = "tte.dist - prior.dist combination",
      y = "Run Time (min)",
      title = "Execution time in minutes"
    ) +
    ggplot2::theme_minimal()
  
  return(list(summary = time.summaries,     # for overview
              plot = p,                     # for option to take plot as is and manipulate if further
              df = time.df  # for option to plot manually in preferred format
              ))
}



## END OF DOC
