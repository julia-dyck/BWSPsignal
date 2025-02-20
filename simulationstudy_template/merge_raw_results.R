#### merge raw results to one table ____________________________________________


#### load all existing simulation results --------------------------------------

### result path
resultpath = paste0(getwd(), "/results_raw")

prog = sim.monitor.progress(pc_table = pc.numeric, 
                            wd = resultpath,
                            batch_max = 10)
res.ind = which(prog == 10)

pc.finished = pc.num[res.ind,]

#### merged results ------------------------------------------------------------

# if already merged, load merged result table with
load(file = paste0(resultpath, "/merged_res.RData"))

# else merge and save with
res = pc.finished %>%
  sim.merge.results(pc_table =.,
                    wd_load = resultpath,
                    wd_save = resultpath) %>%
  as_tibble() %>%
  mutate(lab = ifelse(.$adr.rate > 0, 1, 0)) # generate labels for signal

save(res, file = paste0(resultpath, "/merged_res.RData"))


## END OF DOC
