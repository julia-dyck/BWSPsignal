#' Derive ROPEs from setup parameter combinations in simulation study
#' 
#' 
#' 


eval.calc_rope = function(cred.levels, res.row){
  # rope.infos.vec is a row vector containing tte.dist, prior.dist, and prior means & sds for all shape pars
  
  tte.dist = rope.infos.vect$tte.dist     # for if question
  prior.dist = rope.infos.vect$prior.dist # for if question
   
  rope.percentile.l = (1-cred.levels)/2
  rope.percentile.u = cred.levels + (1-cred.levels)/2
  rope.perc = cbind(rope.percentile.l, rope.percentile.u) # calc rope for each row
  
  
  if(tte.dist == "w"){
    # w distribution
    m = rope.infos.vect$shape.mean_w
    sd = rope.infos.vect$shape.sd_w
    if(prior.dist == "ll" || prior.dist == "fl"){
      # use logprior_repar
      pars = logprior_repar(m_t = m, s_t = sd)
      ropes = data.frame(t(apply(X = rope.perc, MARGIN = 1, FUN = qlnorm, meanlog =pars[1], sdlog = pars[2])))
      # make a row vector (to be added to one row in res table later on)
      ropes.vect = as.vector(ropes)
      return(list(ropes, ropes.vect)) # have a look at order to correctly generate and add names
    } 
    
  } else if (tte.dist == "dw"){
    # dw distribution
    prior.mean = rope.infos.vect$prior.mean
    prior.sd = rope.infos.vect$prior.sd
  } else if (tte.dist == "pgw"){
    # pgw distribution
    prior.mean = rope.infos.vect$prior.mean
    prior.sd = rope.infos.vect$prior.sd
  }
  
}


eval.calc_rope(cred.levels = c(0.8, 0.9, 0.95), 
              rope.infos.vec = rope.infos[2,])
