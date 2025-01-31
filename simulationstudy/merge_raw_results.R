#### merge raw results to one table ____________________________________________


#### load all existing simulation results --------------------------------------

### result path
resultpath = paste0(getwd(), "/results_raw")

prog = monitor_progress(pc_table = pc.numeric, wd = resultpath)
res.ind = which(prog == 10)

pc.finished = pc.num[res.ind,]

#### merged results ------------------------------------------------------------

# if already merged, load merged result table with
load(file = paste0(resultpath, "/merged_res.RData"))

# else
### HIER WEITER
res = pc.finished %>%
  sim.merge.results(pc_table =.,
                          wd_load = paste0(resultpath, "/merged_res.RData"),
                          wd_save = paste0(resultpath, "/merged_res.RData")) %>%
  as_tibble() %>%
  mutate(lab = ifelse(.$adr.rate > 0, 1, 0)) # generate labels for signal

save(res, file = paste0(resultpath, "/merged_res.RData"))


## END OF DOC
