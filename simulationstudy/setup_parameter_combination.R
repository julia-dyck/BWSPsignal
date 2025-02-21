### parameter combinations (pc) for simulation study ---------------------------

# have a look at `?datagen_tte` for information the pc parameters

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

pc = rbind(pc_no_adr,
           pc_with_adr)

pc = pc[, 8:1]
pc = pc[order(pc$N),]
rownames(pc) <- 1:nrow(pc)

dim(pc)

### generate various pc formats ------------------------------------------------
# (for conduction and evaluation)

# transform characters to numerics
pc.numeric = pc

pc.numeric$dist.ass <- as.numeric(factor(pc$dist.ass))
pc.numeric$adr.ass = as.numeric(factor(pc$adr.ass))


# add label to indicate whether adr effect exists in the simulated scenario
pc.num = pc
pc.num$label = as.numeric(pc.num$adr.rate > 0) 

### save pc in various formats

save(pc, pc.num, pc.numeric, file = "pc_in_various_formats.RData")




## END OF DOC