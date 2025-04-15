#' Evaluate effective sample sizes by tte and prior distribution
#'
#' Summarizes and visualizes effective sample sizes of the stan models fitted during 
#' the simulation study, grouped by time-to-event (tte) and prior distribution types. 
#' This helps assess which of the tte and prior distribution choices can be expected 
#' to be sufficient for HDI+ROPE testing, along with other diagnostics such as 
#' \code{\link{eval.non_conv_cases}} and \code{\link{eval.ex_times}}.
#'
#' @param pc_list list containing simulation parameters (see \code{\link{sim.setup_simpars}})
#' @param threshold numeric threshold for acceptable effective sample size (default: 10000 as recommended by Kruschke, 2015)
#'
#' @return list with summary statistics (`$summary`), a ggplot2 object (`$plot`), 
#' and the cleaned data (`$df`) used for both.
#' 
#' 
#'
#' @export



eval.eff_sample_sizes = function(pc_list, threshold = 10000){
  
  if (!exists("res")) { 
    # obtain res table
    tryCatch({
      load(paste0(pc_list$add$resultpath, "/res.RData"))
      message("res.RData successfully loaded")
    }, error = function(cond) {
      sim.merge_results(pc_list, save = T)
      load(paste0(pc_list$add$resultpath, "/res.RData"))
      message(" batches merged and loaded")
    })
  }
  else{
    message("Object `res` loaded in current environment is used to extract effective sample sizes.")
  }
  
  # NULL values ~> NA
  res$nu.po.n_eff <- lapply(res$nu.po.n_eff, function(x) if (length(x) == 0) NA else x)
  res$ga.po.n_eff <- lapply(res$ga.po.n_eff, function(x) if (length(x) == 0) NA else x)
  
  # Select and rename relevant cols for nu
  n_eff.nu <- dplyr::select(res, tte.dist, prior.dist, nu.po.n_eff)
  n_eff.nu <- dplyr::rename(n_eff.nu, n_eff = nu.po.n_eff)
  #n_eff.nu$n_eff = ifelse(is.null(n_eff.nu$n_eff), NA, n_eff.nu$n_eff)
  n_eff.nu$parameter <- "shape1"
  
  # Select and rename relevant cols for ga
  n_eff.ga <- dplyr::select(res, tte.dist, prior.dist, ga.po.n_eff)
  n_eff.ga <- dplyr::rename(n_eff.ga, n_eff = ga.po.n_eff)
  # Replace NULLs with NAs in a list-column ### HIER WEITER
  res$ga.po.n_eff <- lapply(res$ga.po.n_eff, function(x) if (is.null(x)) NA else x)
  n_eff.ga$parameter <- "shape2"
  
  # Combine into long format
  n_eff.long <- rbind(n_eff.nu, n_eff.ga)

  # Reorder columns
  n_eff.long <- n_eff.long[, c("tte.dist", "prior.dist", "parameter", "n_eff")]
  
  # Unlist all columns to remove any list-columns
  for (col in names(n_eff.long)) {
    n_eff.long[[col]] <- unlist(n_eff.long[[col]])
  }
  n_eff.long$n_eff <- as.numeric(n_eff.long$n_eff)
  
  # execution time dist per tte.dist and prior.dist
  ## WHAT DO WE REALLY NEED; MEAN; NO OF SIMS WITH NEFF < 10000; LOOK AT MANUSCRIPT
  
  n_eff.summaries <- suppressWarnings(
    dplyr::summarise(
      dplyr::group_by(n_eff.long, tte.dist, prior.dist, parameter),
      # min = min(n_eff, na.rm = TRUE),
      # first_qu = quantile(n_eff, 0.25, na.rm = TRUE),
      # median = median(n_eff, na.rm = TRUE),
      # mean = mean(n_eff, na.rm = TRUE),
      # third_qu = quantile(n_eff, 0.75, na.rm = TRUE),
      # max = max(n_eff, na.rm = TRUE),
      prob.above.thr = sum(n_eff > threshold, na.rm = TRUE) / dplyr::n(),
      .groups = "drop"
    )
  )
  
  #return(n_eff.summaries)
  
  # p = ggplot2::ggplot(n_eff.long, ggplot2::aes(x = prior.dist, y = n_eff, fill = parameter)) +
  #   ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.8)) +
  #   ggplot2::facet_wrap(~ tte.dist, ncol = 1) +
  #   ggplot2::labs(x = "prior.dist", y = "n_eff grouped by tte.dist", fill = "parameter") +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  # 
  
  n_eff.long$group <- interaction(n_eff.long$tte.dist, n_eff.long$prior.dist, sep = " - ")
  
  p <- ggplot2::ggplot(n_eff.long, ggplot2::aes(x = group, y = n_eff, fill = parameter)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.8), width = 0.5) +
    ggplot2::geom_hline(yintercept = 10000, linetype = "dashed", color = "black", size = 1) +
    ggplot2::labs(
      x = "tte.dist - prior.dist combination",
      y = "n_eff",
      title = "Effective sample sizes",
      fill = "Parameter"
    ) +
    ggplot2::theme_minimal() +
    
    ggplot2::theme(legend.position = "top")
  
  return(list(summary = n_eff.summaries,     # for overview
              plot = p,                     # for option to take plot as is and manipulate if further
              df = n_eff.long  # for option to plot manually in preferred format
  ))
}





## END OF DOC
