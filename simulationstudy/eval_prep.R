#### evaluation of simstudy01 - prep ####

#### prep working directory setup depending on device --------------------------
if(Sys.info()["nodename"] == "UWIW-386CCC2"){              # if access from office tower
  wd_output = "C:/Users/jdyck/sciebo/bADR_simstudyres01"
  wd_pkg = "C:/Users/jdyck/office_tower/bADRfcts"
}
if(Sys.info()["nodename"] == "UWIW-8B3RL34"){              # if access from office laptop
  wd_output = "C:/Users/jdyck/sciebo/bADR_simstudyres01"
  wd_pkg = "C:/Users/jdyck/github_office_laptop/bADRfcts"
}

#### set working directory to sim result folder --------------------------------
# setwd("D:/J/sciebo/bADR_simstudyres01")
setwd(wd_output)

#### loading of parameter combination table ------------------------------------
load("pc_in_various_formats.RData")

## following loaded already, but initially generated like so:
## create pc.num = table in tibble format with binary signal labels added
#pc.num = as_tibble(pc.numeric)
#colnames(pc.num)[7:8] = c("dist.ass", "adr.ass")
#pc.num = data.frame(pc.num, label = ifelse(pc.num$adr.rate == 0, 0, 1))

#View(pc.num)

#### load all existing simulation results --------------------------------------

prog = monitor_progress(pc_table = pc.numeric, wd = getwd())
res.ind = which(prog == 10)

pc.finished = pc.num[res.ind,]

# res = pc.finished %>%
#   bADRfcts::merge_results(pc_table =.,
#                           wd_load = "C:/Users/jdyck/sciebo/bADR_simstudyres01",
#                           wd_save = "C:/Users/jdyck/sciebo/bADR_simstudyres01/bADR_simstudyres01_merged") %>%
#   as_tibble() %>%
#   mutate(lab = ifelse(.$adr.rate > 0, 1, 0)) # generate labels for signal
#
# save(res, file = "C:/Users/jdyck/sciebo/bADR_simstudyres01/bADR_simstudyres01_merged/merged_res.RData")

# load(file = "D:/J/Sciebo/bADR_simstudyres01/bADR_simstudyres01_merged/merged_res.RData")
load(file = "C:/Users/jdyck/sciebo/bADR_simstudyres01/bADR_simstudyres01_merged/merged_res.RData")


#### reset the working directory
setwd(wd_pkg)


## END OF DOC
