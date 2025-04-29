#' Setup simulation scenarios
#' 
#' The function returns the set of simulation scenarios determined by the data generating process (dgp) parameter
#' values of interest.
#' 
#' @param N A scalar or vector of sample sizes.
#' @param br A scalar or vector of background rates.
#' @param adr.rate A scalar or vector of adverse drug reaction rates.
#' @param adr.when A scalar or vector of expected event times (relative number, e.g. 0.5 matches half of study period).
#' @param adr.when.names A vector of description/short name of expected event times (ie. name of simulated truth) matching the \code{adr.when} vector.
#' @param adr.relsd A scalar or vector of relative standard deviations from the adverse drug reaction times.
#' @param study.period A scalar specifying the length of the study period.
#' 
#' @return A data frame with the simulation scenarios that will be inserted into the 
#' \code{\link{datagen_tte}} function.
#' 
#' @export



#### setup data generating process parameters


sim.setup_dgp_pars = function(N,           # dgp parameters
                              br,
                              adr.rate,
                              adr.when,
                              adr.when.names,
                              adr.relsd,
                              study.period 
                               ){
  if(any(adr.rate == 0)){ # separate control and adr>0 cases
    adr.rate = adr.rate[-(which(adr.rate==0))]
  }
  else{ # its fine, but notify user that control case is necessary
    warning("Control case (adr.rate = 0) necessary for simulation study. Value 0 is added to specified vector adr.rate.")
  }
  
  if(length(adr.when) != length(adr.when.names)){
    stop("Length of adr.when and adr.when.names must be equal.")
  }
  
  # par combis with adr.rate > 0
  pc_with_adr = expand.grid(        
    study.period = study.period,
    adr.relsd = adr.relsd,
    adr.when = adr.when,
    adr.rate = adr.rate, # remove 0 from adr.rate,         
    br = br,
    N = N
  )
  # par combis with adr.rate = 0 (control), adr.when then also set to 0

  pc_no_adr = expand.grid(       
    study.period = study.period,
    adr.relsd = NA,
    adr.when = NA,
    adr.rate = 0,
    br = br,
    N = N
  )
  
  dgp_pc = rbind(pc_no_adr,
             pc_with_adr)
  # dgp_pc$adr.when.names = 
    # match to position of adr.when
    
  dgp_pc = dgp_pc[, ncol(dgp_pc):1]
  dgp_pc = dgp_pc[order(dgp_pc$N),]
  rownames(dgp_pc) <- 1:nrow(dgp_pc)
  return(dgp_pc)
}

# dgp_pc = sim.setup_dgp_pars(N = c(500, 3000, 5000),
#                             br = 0.1,
#                             adr.rate = c(0.5, 1),
#                             adr.when = c(0.25, 0.5, 0.75),
#                             adr.relsd = c(0.05),
#                             study.period = 365)
# 
# dgp_pc

#### model fitting parameter specification

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
        cat("The a priori mean mean value for the shape is set to 1.\n")
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
        cat("The a priori mean mean value for the uncensored Weibull shape is set to 1.\n")
        shape.mean <- "1"
        shape.sd <- readline("What is the a priori standard deviation for the shape?\n ")
        scale_c.mean = readline("What is the a priori mean value for the censored Weibull scale?\n ")
        scale_c.sd = readline("What is the a priori standard deviation for the censored Weibull scale?\n ")
        cat("The a priori mean mean value for the censored Weibull shape is set to 1.\n")
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
        cat("The a priori mean mean value for the shape is set to 1.\n")
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



#### setup test parameters
#'
#'
#'@export

sim.setup_test_pars = function(post.ci.type = c("ETI", "HDI"),
                               cred.level = seq(0.5, 0.95, by = 0.05),
                               sensitivity.option = 1:3){
  
  message("The rope reflecting the null-hypothesis of constant hazard is will be an equal-tailed confidence interval (ETI) based on shape parameters' prior distribution, mean, and sd specified in function sim.setup_fit_pars().")
  
  expand.grid(post.ci.type = post.ci.type,
              cred.level = cred.level,
              sensitivity.option = sensitivity.option)
}

# test_pc = sim.setup_test_pars()

#'
#'
#' @export

sim.setup_sim_pars = function(N,                 # dgp parameters
                              br,                # |
                              adr.rate,          # |
                              adr.when,          # |
                              adr.relsd,         # v
                              study.period,      # -
                              tte.dist,          # tuning parameters
                              prior.belief,      # |
                              prior.dist,        # |
                              post.ci.type,      # |
                              cred.level,        # v
                              sensitivity.option,# -
                                 
                              reps = 100, # additional parameters
                              batch.size = 10,
                              batch.nr = reps/batch.size,
                              resultpath = paste0(getwd(), "/results_raw"),
                              stanmod.chains = 4,
                              stanmod.iter = 11000,
                              stanmod.warmup = 1000
                              ){       
  
  dgp_pars = sim.setup_dgp_pars(N = N,
                                br = br,
                                adr.rate = adr.rate,
                                adr.when = adr.when,
                                adr.relsd = adr.relsd,
                                study.period = study.period)
  
  fit_pars = sim.setup_fit_pars(tte.dist = tte.dist,
                                prior.belief = prior.belief,
                                prior.dist = prior.dist,
                                list.output = T)
  
  test_pars = sim.setup_test_pars(post.ci.type = post.ci.type,
                                  cred.level = cred.level,
                                  sensitivity.option = sensitivity.option)
  
  add_pars = list(reps = reps,
                  batch.size = batch.size,
                  batch.nr = reps/batch.size,
                  resultpath = resultpath,
                  stanmod.chains = stanmod.chains,
                  stanmod.iter = stanmod.iter,
                  stanmod.warmup = stanmod.warmup
                  )
  
  input_args = list(N = N, 
                    br = br, 
                    adr.rate = adr.rate, 
                    adr.when = adr.when, 
                    adr.relsd = adr.relsd, 
                    study.period = study.period,
                    tte.dist = tte.dist, 
                    prior.belief = prior.belief, 
                    prior.dist = prior.dist, 
                    post.ci.type = post.ci.type, 
                    cred.level = cred.level, 
                    sensitivity.option = sensitivity.option,
                    reps = reps, 
                    batch.size = batch.size, 
                    resultpath = resultpath, 
                    stanmod.chains = stanmod.chains, 
                    stanmod.iter = stanmod.iter, 
                    stanmod.warmup = stanmod.warmup
                    )
                  
  cat(paste0("Each combination of sample scenario and prior specification leads to a total of ",
                 nrow(dgp_pars) * sum(nrow(fit_pars$w), nrow(fit_pars$dw), nrow(fit_pars$pgw)),
                 " different simulation settings. ",
                 "Each simulation scenario’s data generation and posterior estimation will be repeated ", 
                 reps,
                 " times. "))
  
  pc_table = list()  # parameters in table format s.t. one row = one simulation scenario
  pc_table$w = dplyr::cross_join(dgp_pars, fit_pars$w[c("tte.dist", "prior.dist", "prior.belief")])
  pc_table$dw = dplyr::cross_join(dgp_pars, fit_pars$dw[c("tte.dist", "prior.dist", "prior.belief")])
  pc_table$dw = dplyr::cross_join(dgp_pars, fit_pars$dw[c("tte.dist", "prior.dist", "prior.belief")])
  
  pc_table = dplyr::bind_rows(pc_table)
  
  
  sim_pars = list(dgp = dgp_pars, fit = fit_pars, test = test_pars, add = add_pars, input = input_args, pc_table = pc_table)
  
  return(sim_pars)
}




#### setup tuning parameters (combine fit_pars and test_pars)


# FOR ROXYGEN DOC


# The tuning parameters are used to evaluate the performance of different BWSP 
# signal detection tests applied to data scenarios of interest.
# The set of tuning parameters consisting of the choice of the

# ## distribution chosen for the tte model, must be a subset out of "w", "dw" 
#    and pgw",

# ## prior belief about an adverse event being and adverse drug reaction, and if 
#    so, when it is expected to occur
#    (this will trigger promts asking for prior means and standard deviations for 
#    all parameters reflecting that belief; have a look at our vignette about 
#    prior elicitation to formalize the prior belief),

# ## prior distribution chosen for scale and shape parameters, must be a subset 
#    out of "fgg", "ggg", "fll" and "lll",

# ## type of posterior credible interval, must be a subset out of "ETI" 
#    (equal-tailed interval) and "HDI" (highest density interval).





## END OF DOC
