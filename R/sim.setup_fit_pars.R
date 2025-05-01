#' model fitting parameter specification
#' 
#' 
#' 
#'
#' @export

sim.setup_fit_pars = function(tte.dist = c("w", "dw", "pgw"),
                              prior.belief = c("none", "beginning", "middle", "end"),
                              prior.dist = c("fgg", "ggg", "fll", "lll"),
                              list.output = F){
  # check whether prior.belief "none" is included (necessary as it also provides the base for ROPE specification)
  if(!any(prior.belief == "none")){
    stop("Prior belief 'none' must be included in prior.belief and reflect the null-hypothesis of constant hazard formalized as `prior mean of all shape parameters = 1`.")
  }
  
  # table of all model fitting cases (without explicit parameter specification)
  dist_pc = expand.grid(tte.dist = tte.dist,
                        prior.dist = prior.dist,
                        prior.belief = prior.belief
  )
  # prepare empty df for each dist
  # w
  belief.df_w = data.frame(tte.dist = character(),
                           prior.belief = character(), 
                           scale.mean_W = numeric(), 
                           scale.sd_w = numeric(), 
                           shape.mean_w = numeric(), 
                           shape.sd_W = numeric())
  
  # dw
  belief.df_dw = data.frame(tte.dist = character(),
                            prior.belief = character(), 
                            scale.mean_dw = numeric(), 
                            scale.sd_dw = numeric(), 
                            shape.mean_dw = numeric(), 
                            shape.sd_dw = numeric(),
                            scale_c.mean_dw = numeric(),
                            scale_c.sd_dw = numeric(),
                            shape_c.mean_dw = numeric(),
                            shape_c.sd_dw = numeric())
  
  # pgw
  belief.df_pgw = data.frame(tte.dist = character(),
                             prior.belief = character(), 
                             scale.mean_pgw = numeric(), 
                             scale.sd_pgw = numeric(), 
                             shape.mean_pgw = numeric(), 
                             shape.sd_pgw = numeric(),
                             powershape.mean_pgw = numeric(),
                             powershape.sd_pgw = numeric())
  
  
  # add explicite parameter prior specification (depending on tte distribution)
  if(sum(tte.dist == "w") > 0){
    
    for(row in 1:length(prior.belief)){
      cat(paste0("Please specify prior mean and standard deviation of the Weibull (w) parameters reflecting the prior belief\n `", prior.belief[row], "`:\n"))
      
      if(prior.belief[row] == "none"){
        cat("The prior belief 'none' reflects the null-hypothesis of constant hazard formalized as `prior mean of all shape parameters = 1`.\n")
        scale.mean = readline("What is the a priori mean value for the scale?\n ")
        scale.sd <- readline("What is the a priori standard deviation for the scale?\n ")
        cat("The a priori mean value for the shape is set to 1.\n")
        shape.mean <- "1"
        shape.sd <- readline("What is the a priori standard deviation for the shape?\n ")
      }
      else{
        scale.mean = readline("What is the a priori mean value for the scale?\n ")
        scale.sd <- readline("What is the a priori standard deviation for the scale?\n ")
        shape.mean <- readline("What is the a priori mean value for the shape?\n ")
        shape.sd <- readline("What is the a priori standard deviation for the shape?\n ")
      }
      
      scale.mean = as.numeric(unlist(strsplit(scale.mean, ",")))
      scale.sd = as.numeric(unlist(strsplit(scale.sd, ",")))
      shape.mean = as.numeric(unlist(strsplit(shape.mean, ",")))
      shape.sd = as.numeric(unlist(strsplit(shape.sd, ",")))
      
      belief.df_w = rbind(belief.df_w, 
                          data.frame(tte.dist = "w",
                                     prior.belief = prior.belief[row], 
                                     scale.mean_w = scale.mean, 
                                     scale.sd_w = scale.sd, 
                                     shape.mean_w = shape.mean, 
                                     shape.sd_w = shape.sd) 
      )
    }
    
  } 
  
  if(sum(tte.dist == "dw") > 0){
    
    for(row in 1:length(prior.belief)){
      cat(paste0("Please specify prior mean and standard deviation of the Weibull & censored Weibull (dw) parameters reflecting the prior belief\n `", prior.belief[row], "`:\n"))
      
      if(prior.belief[row] == "none"){
        cat("The prior belief 'none' reflects the null-hypothesis of constant hazard formalized as `prior mean of all shape parameters = 1`.\n")
        scale.mean = readline("What is the a priori mean value for the scale?\n ")
        scale.sd <- readline("What is the a priori standard deviation for the scale?\n ")
        cat("The a priori mean value for the uncensored Weibull shape is set to 1.\n")
        shape.mean <- "1"
        shape.sd <- readline("What is the a priori standard deviation for the shape?\n ")
        scale_c.mean = readline("What is the a priori mean value for the censored Weibull scale?\n ")
        scale_c.sd = readline("What is the a priori standard deviation for the censored Weibull scale?\n ")
        cat("The a priori mean value for the censored Weibull shape is set to 1.\n")
        shape_c.mean = "1"
        shape_c.sd = readline("What is the a priori standard deviation for the censored Weibull shape?\n ")
      }
      else{
        scale.mean = readline("What is the a priori mean value for the uncensored Weibull scale?\n ")
        scale.sd = readline("What is the a priori standard deviation for the uncensored Weibull scale?\n ")
        shape.mean = readline("What is the a priori mean value for the uncensored Weibull shape?\n ")
        shape.sd = readline("What is the a priori standard deviation for the uncensored Weibull shape?\n ")
        scale_c.mean = readline("What is the a priori mean value for the censored Weibull scale?\n ")
        scale_c.sd = readline("What is the a priori standard deviation for the censored Weibull scale?\n ")
        shape_c.mean = readline("What is the a priori mean value for the censored Weibull shape?\n ")
        shape_c.sd = readline("What is the a priori standard deviation for the censored Weibull shape?\n ")
      }
      
      scale.mean = as.numeric(unlist(strsplit(scale.mean, ",")))
      scale.sd = as.numeric(unlist(strsplit(scale.sd, ",")))
      shape.mean = as.numeric(unlist(strsplit(shape.mean, ",")))
      shape.sd = as.numeric(unlist(strsplit(shape.sd, ",")))
      scale_c.mean = as.numeric(unlist(strsplit(scale_c.mean, ",")))
      scale_c.sd = as.numeric(unlist(strsplit(scale_c.sd, ",")))
      shape_c.mean = as.numeric(unlist(strsplit(shape_c.mean, ",")))
      shape_c.sd = as.numeric(unlist(strsplit(shape_c.sd, ",")))
      
      belief.df_dw = rbind(belief.df_dw, 
                           data.frame(tte.dist = "dw",
                                      prior.belief = prior.belief[row], 
                                      scale.mean_dw = scale.mean, 
                                      scale.sd_dw = scale.sd, 
                                      shape.mean_dw = shape.mean, 
                                      shape.sd_dw = shape.sd,
                                      scale_c.mean_dw = scale_c.mean, 
                                      scale_c.sd_dw = scale_c.sd, 
                                      shape_c.mean_dw = shape_c.mean, 
                                      shape_c.sd_dw = shape_c.sd
                           ) 
      )
    }
  }
  
  if(sum(tte.dist == "pgw") > 0){
    
    for(row in 1:length(prior.belief)){
      cat(paste0("Please specify prior mean and standard deviation of the Power generalized Weibull (pgw) parameters reflecting the prior belief\n `", prior.belief[row], "`:\n"))
      
      if(prior.belief[row] == "none"){
        cat("The prior belief 'none' reflects the null-hypothesis of constant hazard formalized as `prior mean of all shape parameters = 1`.\n")
        scale.mean = readline("What is the a priori mean value for the scale?\n ")
        scale.sd = readline("What is the a priori standard deviation for the scale?\n ")
        cat("The a priori mean value for the shape is set to 1.\n")
        shape.mean = "1"
        shape.sd = readline("What is the a priori standard deviation for the shape?\n ")
        cat("The a priori mean mean value for the powershape is set to 1.\n")
        powershape.mean = "1"
        powershape.sd = readline("What is the a priori standard deviation for the powershape?\n ")
      }
      else{
        scale.mean = readline("What is the a priori mean value for the scale?\n ")
        scale.sd = readline("What is the a priori standard deviation for the scale?\n ")
        shape.mean = readline("What is the a priori mean value for the shape?\n")
        shape.sd = readline("What is the a priori standard deviation for the shape?\n ")
        powershape.mean = readline("What is the a priori mean value for the powershape?\n ")
        powershape.sd = readline("What is the a priori standard deviation for the powershape?\n ")
      }
      
      scale.mean = as.numeric(unlist(strsplit(scale.mean, ",")))
      scale.sd = as.numeric(unlist(strsplit(scale.sd, ",")))
      shape.mean = as.numeric(unlist(strsplit(shape.mean, ",")))
      shape.sd = as.numeric(unlist(strsplit(shape.sd, ",")))
      powershape.mean = as.numeric(unlist(strsplit(powershape.mean, ",")))
      powershape.sd = as.numeric(unlist(strsplit(powershape.sd, ",")))
      
      belief.df_pgw = rbind(belief.df_pgw, 
                            data.frame(tte.dist = "pgw",
                                       prior.belief = prior.belief[row], 
                                       scale.mean_pgw = scale.mean, 
                                       scale.sd_pgw = scale.sd, 
                                       shape.mean_pgw = shape.mean, 
                                       shape.sd_pgw = shape.sd,
                                       powershape.mean_pgw = powershape.mean,
                                       powershape.sd_pgw = powershape.sd
                            ) 
      )
    }
    
  } 
  
  
  fit_pc_w = dplyr::inner_join(dist_pc, belief.df_w, by = c("tte.dist", "prior.belief"))
  fit_pc_w$prior.dist = as.character(fit_pc_w$prior.dist)
  fit_pc_dw = dplyr::inner_join(dist_pc, belief.df_dw, by = c("tte.dist", "prior.belief"))
  fit_pc_dw$prior.dist = as.character(fit_pc_dw$prior.dist)
  fit_pc_pgw = dplyr::inner_join(dist_pc, belief.df_pgw, by = c("tte.dist", "prior.belief"))
  fit_pc_pgw$prior.dist = as.character(fit_pc_pgw$prior.dist)
  
  fit_pc = list(w = fit_pc_w, dw = fit_pc_dw, pgw = fit_pc_pgw)
  if(list.output == F){
    return(dplyr::bind_rows(fit_pc))
  }
  return(fit_pc)
}

# fit_pc = sim.setup_fit_pars(tte.dist = c("w"), 
#                    prior.dist = c("fgg", "ggg", "fll", "lll"),
#                    prior.belief = c("none", "beginning", "middle", "end"),
#                    list.output = F)
# 
# dim(fit_pc)
# View(fit_pc)


## END OF DOC
