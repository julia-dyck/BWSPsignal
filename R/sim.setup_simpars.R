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
  
  ## TODO prior belief sachen erstmal wieder raus, fälle seperat aufsetzen und 
  ## in sim.setup_sim_scenarios oder so mergen
  pc_with_adr = expand.grid(
    adr.ass = c("beginning", "middle", "end", "none"),
    dist.ass = c("fix.gam.gam", "gam.gam.gam", "fix.log.log", "log.log.log"),
    study.period = 365,
    adr.relsd = c(0.05),
    adr.when = c(0.25, 0.5, 0.75),
    adr.rate = c(0.5, 1),         
    br = 0.1,
    N = c(500, 3000, 5000)
  )
  
  pc_no_adr = expand.grid(
    adr.ass = c("beginning", "middle", "end", "none"),
    dist.ass = c("fix.gam.gam", "gam.gam.gam", "fix.log.log", "log.log.log"),
    study.period = 365,
    adr.relsd = c(0.05),
    adr.when = c(0),
    adr.rate = c(0),
    br = 0.1,
    N = c(500, 3000, 5000)
  )
}



sim.setup_mod_pars = function(mod,
                              prior.dist,
                              df.of.believes){
  ## sets up an expand.grid matrix with cols:
  # mod, prior.dist, belief.abbreviation
  # each belief.abbreviation is linked to one row of df.of.believes, that then again contains all prior means and sds
}