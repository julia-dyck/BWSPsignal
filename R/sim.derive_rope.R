#' Derive ROPEs from setup parameter combinations in simulation study
#' 
#' 
#' 



## to be adjusted later when i go through evaluation workflow

sim.derive_rope = function(fit_pc){
  # derive rope from fit_pc
  # ACHTUNG: fit_pc is now a list of 3 dfs (one per tte.dist)
  rope = fit_pc %>% 
    dplyr::filter(prior.belief == "none") %>% 
    dplyr::select(tte.dist, prior.belief, scale.mean_w, scale.sd_w, shape.mean_w, shape.sd_w,
                  scale.mean_dw, scale.sd_dw, shape.mean_dw, shape.sd_dw,
                  scale_c.mean_dw, scale_c.sd_dw, shape_c.mean_dw, shape_c.sd_dw,
                  scale.mean_pgw, scale.sd_pgw, shape.mean_pgw, shape.sd_pgw, powershape.mean_pgw, powershape.sd_pgw)
  
  return(rope)
}
