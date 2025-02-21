#' Setup simulation scenarios
#' 
#' 
#' 






sim.setup_dgp_pars = function(N,           # dgp parameters
                              br,
                              adr.rate, # without control 0
                              adr.relsd,
                              study.period
                               ){
  
  ## TODO prior belief sachen erstmal wieder raus, fälle seperat aufsetzen und 
  ## 
  pc_with_adr = expand.grid(
    study.period = study.period,
    adr.relsd = adr.relsd,
    adr.rate = adr.rate,         
    br = br,
    N = N
  )
  
  pc_no_adr = expand.grid(
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





sim.setup_fit_pars = function(tte.dist,
                              prior.dist,
                              prior.belief,  # ehemals weniger allgemein "adr.when"
                              scale.mean,
                              shape.mean){
  fit_pc = expand.grid(tte.dist, prior.dist, prior.belief)
  fit_pc = fit_pc[order(fit_pc$tte.dist),] # FIX ERROR
  ## sets up an expand.grid matrix with cols:
  
}

sim.setup_fit_pars(tte.dist = c("w", "pgw"),
                   prior.dist = c("fgg", "ggg", "fll", "lll"),
                   prior.belief = c("none", "beginning", "middle", "end"))


df_of_belief = data.frame(belief.description = c("none", "beginning", "middle", "end"),
                          w_scale.mean_w)

dev_fit = sim.setup_fit_pars(mod = "pgw",
                             prior.dist = c("fgg", "ggg", "fll", "lll"),
                             df.of.believes = NULL)


sim.setup_test_pars = function(){
  
}