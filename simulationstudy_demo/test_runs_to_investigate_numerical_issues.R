###test modelling with chosen priors 


dat = datagen_tte(pc_list$dgp[3,])

# test run for w 
# under "none" prior
standat = tte2priordat_w(dat, scale.mean = 1, scale.sd = 10, shape.mean = 1, shape.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fg")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "gg")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fl")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "ll")
# under "beginning" prior
standat = tte2priordat_w(dat, scale.mean = 1, scale.sd = 10, shape.mean = 0.207, shape.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fg")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "gg")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fl")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "ll")
# under "middle" prior
standat = tte2priordat_w(dat, scale.mean = 180, scale.sd = 10, shape.mean = 1, shape.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fg") # ERROR
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "gg")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fl") # ERROR
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "ll")
# test run for w under "end" prior
standat = tte2priordat_w(dat, scale.mean = 300, scale.sd = 10, shape.mean = 4, shape.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fg") # ERROR
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "gg")
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "fl") # ERROR
mod = fit_mod_tte(datstan = standat, tte.dist = "w", prior.dist = "ll")

# -> fl and fg problematic (probably due to inflexibility of scale parameter)
# -> perform simulation only on ll and gg

# test run for dw
# under "none" prior
standat = tte2priordat_dw(dat, scale.mean = 1, scale.sd = 10, shape.mean = 1, shape.sd = 10, 
                          scale_c.mean = 1, scale_c.sd = 10, shape_c.mean = 1, shape_c.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "fg")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "gg")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "fl")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "ll")
# under "beginning" prior
standat = tte2priordat_dw(dat, scale.mean = 1, scale.sd = 10, shape.mean = 0.207, shape.sd = 10, 
                          scale_c.mean = 1, scale_c.sd = 10, shape_c.mean = 0.207, shape_c.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "fg")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "gg")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "fl")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "ll")
# under "middle" prior
standat = tte2priordat_dw(dat, scale.mean = 180, scale.sd = 10, shape.mean = 1, shape.sd = 10, 
                          scale_c.mean = 100, scale_c.sd = 10, shape_c.mean = 4, shape_c.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "fg") # WARNINGS about divergent transitions
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "gg")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "fl")
mod = fit_mod_tte(datstan = standat, tte.dist = "dw", prior.dist = "ll")
