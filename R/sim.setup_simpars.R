#' Setup simulation scenarios
#' 
#' 
#' 






sim.setup_dgp_pars = function(N,           # dgp parameters
                              br,
                              adr.rate, 
                              adr.relsd,
                              study.period
                               ){

  if(sum(adr.rate == 0) = 0){ # not rely on user to include controll case
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
                              prior.dist){
  
  if(sum(tte.dist == "w") > 0){
    # prepare empty df
    belief.df_w = data.frame(tte.dist = character(),
                             prior.belief = character(), 
                             scale.mean_W = numeric(), 
                             scale.sd_w = numeric(), 
                             shape.mean_w = numeric(), 
                             shape.sd_W = numeric())
    
    for(row in 1:length(prior.belief)){
      cat(paste0("Please specify prior mean and standard deviation of the Weibull parameters reflecting the prior belief\n `", prior.belief[row], "`:\n"))
      
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
  if(sum(tte.dist == "pgw") > 0){
    # prepare empty df
    belief.df_pgw = data.frame(tte.dist = character(),
                               prior.belief = character(), 
                           scale.mean_pgw = numeric(), 
                           scale.sd_pgw = numeric(), 
                           shape.mean_pgw = numeric(), 
                           shape.sd_pgw = numeric(),
                           powershape.mean_pgw = numeric(),
                           powershape.sd_pgw = numeric())
    for(row in 1:length(prior.belief)){
      cat(paste0("Please specify prior mean and standard deviation of the Power generalized Weibull parameters reflecting the prior belief\n `", prior.belief[row], "`:\n"))
      
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
 
  # } else if(tte.dist == "pgw"){
  #   pgw_scale.mean_pgw = c(0.5, 1, 2, 3)
  #   pgw_scale.sd_pgw = c(0.1, 0.2, 0.3, 0.4)
  #   pgw_scale = data.frame(pgw_scale.mean_pgw, pgw_scale.sd_pgw)
  } else {
    stop("tte.dist not recognized")
  }
  
  dist_pc = expand.grid(tte.dist = tte.dist,
                       prior.dist = prior.dist,
                       prior.belief = prior.belief
                       )  # and then some_join from dyplr
  fit_pc_w = dplyr::left_join(dist_pc, belief.df_w, by = c("tte.dist", "prior.belief"))
  fit_pc_pgw = dplyr::left_join(dist_pc, belief.df_pgw, by = c("tte.dist", "prior.belief"))
  
  #fit_pc = fit_pc[order(fit_pc$tte.dist),]
  ## sets up an expand.grid matrix with cols:
  #print(fit_pc)
  return(list(fit_pc_w, fit_pc_pgw))
}

test_fit = sim.setup_fit_pars(tte.dist = c("w", "pgw"),
                   prior.dist = c("fgg", "ggg", "fll", "lll"),
                   prior.belief = c("none", "beginning"))

test_fit

df_of_belief = data.frame(belief.description = c("none", "beginning", "middle", "end"),
                          w_scale.mean_w)

dev_fit = sim.setup_fit_pars(mod = "pgw",
                             prior.dist = c("fgg", "ggg", "fll", "lll"),
                             df.of.believes = NULL)


sim.setup_test_pars = function(){
  
}