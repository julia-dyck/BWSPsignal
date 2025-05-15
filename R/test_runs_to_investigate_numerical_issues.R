###test modelling with chosen priors 


## test run for w, fg under priors
dat = datagen_tte(pc_list$pc_table[3,])
standat = tte2priordat_w(dat, 
                         scale.mean = 300,
                         scale.sd = 10,
                         shape.mean = 4,
                         shape.sd = 10)
mod = fit_mod_tte(datstan = standat, tte.dist = "w",
                  prior.dist = "fg")
