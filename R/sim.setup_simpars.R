#' Setup simulation scenarios
#' 
#' 
#' 



#### setup data generating process parameters


sim.setup_dgp_pars = function(N,           # dgp parameters
                              br,
                              adr.rate, 
                              adr.relsd,
                              study.period
                               ){

  if(sum(adr.rate == 0) == 0){ # not rely on user to include control case
    adr.rate = c(0, adr.rate)
    warning("No control case included. Adding 0 to adr.rate.")
  }
  
  pc_with_adr = expand.grid(        # par combis with adr
    study.period = study.period,
    adr.relsd = adr.relsd,
    adr.rate = adr.rate,         
    br = br,
    N = N
  )
  
  pc_no_adr = expand.grid(       # par combis without adr (control)
    study.period = study.period,
    adr.relsd = adr.relsd,
    adr.rate = 0,
    br = br,
    N = N
  )
  
  dgp_pc = rbind(pc_no_adr,
             pc_with_adr)
  
  dgp_pc = dgp_pc[, 5:1]
  dgp_pc = dgp_pc[order(dgp_pc$N),]
  rownames(dgp_pc) <- 1:nrow(dgp_pc)
  return(dgp_pc)
}

dev_pc = sim.setup_dgp_pars(N = c(500, 3000, 5000),
                            br = 0.1,
                            adr.rate = c(0.5,1),
                            adr.relsd = 0.05,
                            study.period = 365)



#### model fitting parameter specification

sim.setup_fit_pars = function(tte.dist,
                              prior.belief,
                              prior.dist,
                              list.output = T){
  
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
      
      scale.mean = readline("What is the a priori mean value for the scale?\n")
      scale.sd <- readline("What is the a priori standard deviation for the scale?\n")
      shape.mean <- readline("What is the a priori mean value for the shape?\n")
      shape.sd <- readline("What is the a priori standard deviation for the shape?\n")
      
      scale.mean = as.numeric(unlist(strsplit(scale.mean, ",")))
      scale.sd = as.numeric(unlist(strsplit(scale.sd, ",")))
      shape.mean = as.numeric(unlist(strsplit(shape.mean, ",")))
      shape.sd = as.numeric(unlist(strsplit(shape.sd, ",")))
      
      belief.df_w = rbind(belief.df_w, 
                 data.frame(tte.dist = "w",
                            prior.belief = prior.belief[row], 
                            scale.mean_w = scale.mean, 
                            scale.sd_W = scale.sd, 
                            shape.mean_W = shape.mean, 
                            shape.sd_w = shape.sd) 
                )
    }

  } 
  
  if(sum(tte.dist == "dw") > 0){
    
    for(row in 1:length(prior.belief)){
      cat(paste0("Please specify prior mean and standard deviation of the Weibull & censored Weibull (dw) parameters reflecting the prior belief\n `", prior.belief[row], "`:\n"))
      
      scale.mean = readline("What is the a priori mean value for the uncensored Weibull scale?\n")
      scale.sd = readline("What is the a priori standard deviation for the uncensored Weibull scale?\n")
      shape.mean = readline("What is the a priori mean value for the uncensored Weibull shape?\n")
      shape.sd = readline("What is the a priori standard deviation for the uncensored Weibull shape?\n")
      scale_c.mean = readline("What is the a priori mean value for the censored Weibull scale?\n")
      scale_c.sd = readline("What is the a priori standard deviation for the censored Weibull scale?\n")
      shape_c.mean = readline("What is the a priori mean value for the censored Weibull shape?\n")
      shape_c.sd = readline("What is the a priori standard deviation for the censored Weibull shape?\n")
      
      scale.mean = as.numeric(unlist(strsplit(scale.mean, ",")))
      scale.sd = as.numeric(unlist(strsplit(scale.sd, ",")))
      shape.mean = as.numeric(unlist(strsplit(shape.mean, ",")))
      shape.sd = as.numeric(unlist(strsplit(shape.sd, ",")))
      scale_c.mean = as.numeric(unlist(strsplit(scale_c.mean, ",")))
      scale_c.sd = as.numeric(unlist(strsplit(scale_c.sd, ",")))
      shape_c.mean = as.numeric(unlist(strsplit(shape_c.mean, ",")))
      shape_c.sd = as.numeric(unlist(strsplit(shape_c.sd, ",")))
      
      belief.df_dw = rbind(belief.df_dw, 
                            data.frame(tte.dist = "pgw",
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
      
      scale.mean = readline("What is the a priori mean value for the scale?\n")
      scale.sd = readline("What is the a priori standard deviation for the scale?\n")
      shape.mean = readline("What is the a priori mean value for the shape?\n")
      shape.sd = readline("What is the a priori standard deviation for the shape?\n")
      powershape.mean = readline("What is the a priori mean value for the powershape?\n")
      powershape.sd = readline("What is the a priori standard deviation for the powershape?\n")
      
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
  fit_pc_dw = dplyr::inner_join(dist_pc, belief.df_dw, by = c("tte.dist", "prior.belief"))
  fit_pc_pgw = dplyr::inner_join(dist_pc, belief.df_pgw, by = c("tte.dist", "prior.belief"))
  
  fit_pc = list(w = fit_pc_w, dw = fit_pc_dw, pgw = fit_pc_pgw)
  
  return(fit_pc)
}

test_fit = sim.setup_fit_pars(tte.dist = c("w"), ## TEST WITH ONE; TWO; THREE DISTS
                   prior.dist = c("fgg", "ggg", "fll", "lll"),
                   prior.belief = c("none", "beginning"))

test_fit

df_of_belief = data.frame(belief.description = c("none", "beginning", "middle", "end"),
                          w_scale.mean_w)

dev_fit = sim.setup_fit_pars(mod = "pgw",
                             prior.dist = c("fgg", "ggg", "fll", "lll"),
                             df.of.believes = NULL)



#### setup test parameters

sim.setup_test_pars = function(){
  
}