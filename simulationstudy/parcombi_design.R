### parameter combinations


# just a grid of all combos = full factorial design
pc_fu = expand.grid(
  adr.ass = c("beginning", "middle", "end", "none"),
  dist.ass = c("fix.log.log", "log.log.log"),
  study.period = 365,
  adr.relsd = c(0.05),
  adr.when = c(0.25, 0.5, 0.75),
  adr.rate = c(0, 0.5, 1),       # not sure, yet, whether to include 0.5
  br = 0.1,
  N = c(500, 3000, 5000)
)
pc_fu = pc_fu[, 8:1]



# grid excluding adr=0 and adr.when combos = fractional factorial design
# only one "adr.when" needed as placeholder for "never"

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
dim(pc_fu)
dim(pc_fr)

# testing space ----------------------------------------------------------------

pc_test = expand.grid(
  adr.ass = c("beginning"),
  dist.ass = c("log.log.log"),
  study.period = 365,
  adr.relsd = c(0.05),
  adr.when = c(0.25),
  adr.rate = c(0,1),
  br = 0.1,
  N = c(100)
)

pc_test = pc_test[,8:1]

pc_test


teststats = sim.fit.to.1.sample(pc = pc_test[2,], cores = 2)
View(teststats)


testbatch = sim.repeat.1.scenario(batch.nr = 1, reps=2, pc = pc_fr[7,], cores = 4)

View(testbatch)

#
setwd("D:/Sciebo/bADR_simstudyres01")
testsave = sim.repeat.1.scenario(batch.nr=99, reps = 2, pc = pc_fr[24,], cores=4,
                                 save = T, path = getwd())

# within one batch recruit 1 or 4 kernels
# NEXT: write a loop to produce multiple batches each after another
# NEXT: loop over scenarios maybe
# or: open one session per scenario, use 4 kernels
# number of sessions on a machine: amount of kernels /4
