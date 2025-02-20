#### investigate which scenarios did not converge how often

# REMARK: for new simulations, adjust 
#         - runs.per.sc
#         - all pc.notrun.[dist abbreviation] objects
#         - all total.notrun.[dist abbreviation] objects
#         - all prop.notrun.[dist abbreviation] objects
#         - entries of the dataframe nonconv.tab


#### prep. for calculating relative proportions of not converged runs ----------
runs.per.sc = 100 # 100 runs per scenario
sim.runs.overall = nrow(pc)*runs.per.sc # total no of simulations

# only parameter characterizing the signal detection test dist.ass
unique(pc$dist.ass) # 4 prior distributional choices
no.dist.ass = length(unique(pc$dist.ass))

# total no of simulations per prior dist. choice
sim.runs.one.dist.ass = sim.runs.overall/no.dist.ass 

# table with pcs and no of run batches
pc.prog = cbind(pc, prog) 


#### calculate number and proportion of cancelled simulations ------------------

## prior distributional choice: fix-log-log
pc.notrun.fll = pc.prog %>%
  filter(dist.ass == "fix.log.log", prog < 10) %>%
  mutate(notrun = (10 - prog)*10)

# absolute number of not converged fll runs
total.notrun.fll = pc.notrun.fll %>% select(notrun) %>% sum() 

# relative proportion of not converged fll runs
prop.notrun.fll = pc.notrun.fll %>% select(notrun) %>% sum()/sim.runs.one.dist.ass 


## prior distributional choice: log-log-log
pc.notrun.lll = pc.prog %>%
  filter(dist.ass == "log.log.log", prog < 10) %>%
  mutate(notrun = (10 - prog)*10)

# absolute number of not converged lll runs
total.notrun.lll = pc.notrun.lll %>% select(notrun) %>% sum() 

# relative proportion of not converged fll runs
prop.notrun.lll = pc.notrun.lll %>% select(notrun) %>% sum()/sim.runs.one.dist.ass 


## prior distributional choice: fix-gam-gam
pc.notrun.fgg = pc.prog %>%
  filter(dist.ass == "fix.gam.gam", prog < 10) %>%
  mutate(notrun = (10 - prog)*10)

# absolute number of not converged fgg runs
total.notrun.fgg = pc.notrun.fgg %>% select(notrun) %>% sum() 

# relative proportion of not converged fgg runs
prop.notrun.fgg = pc.notrun.fgg %>% select(notrun) %>% sum()/sim.runs.one.dist.ass 

## prior distributional choice: gam-gam-gam
pc.notrun.ggg = pc.prog %>%
  filter(dist.ass == "gam.gam.gam", prog < 10) %>%
  mutate(notrun = (10 - prog)*10)

# absolute number of not converged ggg runs
total.notrun.ggg = pc.notrun.ggg %>% select(notrun) %>% sum() 

# relative proportion of not converged ggg runs
prop.notrun.ggg = pc.notrun.ggg %>% select(notrun) %>% sum()/sim.runs.one.dist.ass



#### summary table for better overview -----------------------------------------

nonconv.tab = data.frame(dist.ass = unique(pc$dist.ass), 
                         total.no.runs.planned = rep(sim.runs.one.dist.ass,4),
                         total.notrun = c(total.notrun.fll, 
                                          total.notrun.lll, 
                                          total.notrun.fgg, 
                                          total.notrun.ggg),
                          prop.notrun = c(prop.notrun.fll, 
                                          prop.notrun.lll, 
                                          prop.notrun.fgg, 
                                          prop.notrun.ggg)
   )

nonconv.tab

