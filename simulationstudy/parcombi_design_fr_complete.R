### parcombi design with gamma dist assumptions (for completeness)
###
### to consider: fll = 1, lll = 2 already given
###              fgg = 3, ggg = 4 to be implemented

# grid excluding adr=0 and adr.when combos = fractional factorial design
# only one "adr.when" needed as placeholder for "never"


# log normal half:
pc_with_adr = expand.grid(
  adr.ass = c("beginning", "middle", "end", "none"),
  dist.ass = c("fix.log.log", "log.log.log"),
  study.period = 365,
  adr.relsd = c(0.05),
  adr.when = c(0.25, 0.5, 0.75),
  adr.rate = c(0.5, 1),          # not sure, yet, whether to include 0.5
  br = 0.1,
  N = c(500, 3000, 5000)
)

pc_no_adr = expand.grid(
  adr.ass = c("beginning", "middle", "end", "none"),
  dist.ass = c("fix.log.log", "log.log.log"),
  study.period = 365,
  adr.relsd = c(0.05),
  adr.when = c(0),
  adr.rate = c(0),
  br = 0.1,
  N = c(500, 3000, 5000)
)

pc_fr = rbind(pc_no_adr,
              pc_with_adr)

pc_fr = pc_fr[, 8:1]
pc_fr = pc_fr[order(pc_fr$N),]
rownames(pc_fr) <- 1:nrow(pc_fr)
dim(pc_fr)


# gamma half

pc_with_adr = expand.grid(
  adr.ass = c("beginning", "middle", "end", "none"),
  dist.ass = c("fix.gam.gam", "gam.gam.gam"),
  study.period = 365,
  adr.relsd = c(0.05),
  adr.when = c(0.25, 0.5, 0.75),
  adr.rate = c(0.5, 1),          # not sure, yet, whether to include 0.5
  br = 0.1,
  N = c(500, 3000, 5000)
)

pc_no_adr = expand.grid(
  adr.ass = c("beginning", "middle", "end", "none"),
  dist.ass = c("fix.gam.gam", "gam.gam.gam"),
  study.period = 365,
  adr.relsd = c(0.05),
  adr.when = c(0),
  adr.rate = c(0),
  br = 0.1,
  N = c(500, 3000, 5000)
)

pc_fr2 = rbind(pc_no_adr,
              pc_with_adr)

pc_fr2 = pc_fr2[, 8:1]
pc_fr2 = pc_fr2[order(pc_fr2$N),]
rownames(pc_fr2) <- (nrow(pc_fr2)+1):(2*nrow(pc_fr2))
dim(pc_fr2)

pc = rbind(pc_fr, pc_fr2)
str(pc)
