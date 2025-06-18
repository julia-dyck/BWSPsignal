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
  sds.filler = rep(NA, length(pb))
  if(!is.null(prior.sds)){
    sds.filler = rep(prior.sds, length(pb))
  }
  
  prior_list = list()
  prior_list$w = data.frame(prior.belief = pb, 
                            scale.mean_w = placeholder, 
                            scale.sd_w = sds.filler, 
                            shape.mean_w = placeholder, 
                            shape.sd_w = sds.filler)
  # if "w" is not in tte.dist, remove rows 1:4 from dataframe not using dplyr
  if(!("w" %in% tte.dist)){
    prior_list$w = prior_list$w[0,]
  }
  
  # dw
  prior_list$dw = data.frame(prior.belief = pb, 
                            scale.mean_dw = placeholder, 
                            scale.sd_dw = sds.filler, 
                            shape.mean_dw = placeholder, 
                            shape.sd_dw = sds.filler,
                            scale_c.mean_dw = placeholder,
                            scale_c.sd_dw = sds.filler,
                            shape_c.mean_dw = placeholder,
                            shape_c.sd_dw = sds.filler)
  if(!("dw" %in% tte.dist)){
    prior_list$dw = prior_list$dw[0,]
  }
  
  # pgw
  prior_list$pgw = data.frame(prior.belief = pb, 
                             scale.mean_pgw = placeholder, 
                             scale.sd_pgw = sds.filler, 
                             shape.mean_pgw = placeholder, 
                             shape.sd_pgw = sds.filler,
                             powershape.mean_pgw = placeholder,
                             powershape.sd_pgw = sds.filler)
  if(!("pgw" %in% tte.dist)){
    prior_list$pgw = prior_list$pgw[0,]
  }
  
  return(prior_list)
}

# sim.priors_template()
# sim.priors_template(prior.sds = 100)
