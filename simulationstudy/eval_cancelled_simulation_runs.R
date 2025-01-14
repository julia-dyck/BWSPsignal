#### investigate which scenarios did not converge how often


sim.runs.overall = nrow(pc)*100 # 100 reps per scenario
sim.runs.overall
# only parameter characterizing the signal detection test so far is dist.ass
# other parameters specify the sample scenario
sim.runs.one.dist.ass = sim.runs.overall/4
sim.runs.one.dist.ass

pc.prog = cbind(pc, prog)

### calculate number and proportion of cancelled simulations
### presumably due to convergence problems

# (no cases for fix-log-log and log-log-log)

# fix-gam-gam
pc.notrun.fgg = pc.prog %>%
  filter(dist.ass == "fix.gam.gam", prog < 10) %>%
  mutate(notrun = (10 - prog)*10)

pc.notrun.fgg %>% select(notrun) %>% sum() # absolute number of not converged fgg runs

pc.notrun.fgg %>% select(notrun) %>% sum()/sim.runs.one.dist.ass # prop. of not converged fgg runs

# gam-gam-gam
pc.notrun.ggg = pc.prog %>%
  filter(dist.ass == "gam.gam.gam", prog < 10) %>%
  mutate(notrun = (10 - prog)*10)

pc.notrun.ggg %>% select(notrun) %>% sum() # absolute number of not converged fgg runs

pc.notrun.ggg %>% select(notrun) %>% sum()/sim.runs.one.dist.ass # prop. of not converged fgg runs




