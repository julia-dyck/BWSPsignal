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
    adr.relsd = rel.sd,
    adr.rate = 0,
    br = br,
    N = N
  )
  
  dgp_pc = rbind(pc_no_adr,
             pc_with_adr)
  return(dgp_pc)
}


sim.setup_mod_pars = function(mod,
                              prior.dist,
                              df.of.believes  # ehemals weniger allgemein "adr.when"
                              ){
  ## sets up an expand.grid matrix with cols:
  # mod, prior.dist, belief.abbreviation
  # each belief.abbreviation is linked to one row of df.of.believes, that then again contains all prior means and sds
}


sim.setup_test_pars = function(){
  
}