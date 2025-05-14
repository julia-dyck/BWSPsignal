#'
#'
#'
#' @export


# add commentaries about support of prior means and sds (have a look at 
# gamprior_repar, logprior_repar

sim.priors_template = function(tte.dist = c("w", "dw", "pgw"),
                               prior.sds = NULL){
  
  pb = c("none", "beginning", "middle", "end")
  placeholder = rep(NA, length(pb))
  prior_list = list()
  prior_list$w = data.frame(prior.belief = pb, 
                            scale.mean_w = placeholder, 
                            scale.sd_w = placeholder, 
                            shape.mean_w = placeholder, 
                            shape.sd_w = placeholder)
  # if "w" is not in tte.dist, remove rows 1:4 from dataframe not using dplyr
  if(!("w" %in% tte.dist)){
    prior_list$w = prior_list$w[0,]
  }
  
  # dw
  prior_list$dw = data.frame(prior.belief = pb, 
                            scale.mean_dw = placeholder, 
                            scale.sd_dw = placeholder, 
                            shape.mean_dw = placeholder, 
                            shape.sd_dw = placeholder,
                            scale_c.mean_dw = placeholder,
                            scale_c.sd_dw = placeholder,
                            shape_c.mean_dw = placeholder,
                            shape_c.sd_dw = placeholder)
  if(!("dw" %in% tte.dist)){
    prior_list$dw = prior_list$dw[0,]
  }
  
  # pgw
  prior_list$pgw = data.frame(prior.belief = pb, 
                             scale.mean_pgw = placeholder, 
                             scale.sd_pgw = placeholder, 
                             shape.mean_pgw = placeholder, 
                             shape.sd_pgw = placeholder,
                             powershape.mean_pgw = placeholder,
                             powershape.sd_pgw = placeholder)
  if(!("pgw" %in% tte.dist)){
    prior_list$pgw = prior_list$pgw[0,]
  }
  
  if(!is.null(prior.sds)){
    # fill all cols with sd in colname with prior.sds value
    prior_list$w[grepl("sd", names(prior_list$w))] = prior.sds
    prior_list$dw[grepl("sd", names(prior_list$dw))] = prior.sds
    prior_list$pgw[grepl("sd", names(prior_list$pgw))] = prior.sds
  }
  
  return(prior_list)
}

# sim.priors_template()
# sim.priors_template(prior.sds = 100)
