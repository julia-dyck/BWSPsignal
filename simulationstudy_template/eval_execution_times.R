#### evaluate execution times for different prior distributional choices


# calculate execution times for different dist. assumptions
prior.dist.factor = res$prior.dist %>% as.factor()

run.min.numeric = res$run.min %>% as.numeric() # in minutes
run.h.numeric = run.min.numeric/60             # in hours


#### boxplot of execution times ------------------------------------------------

time.df = data.frame(dist.ass.factor, run.min.numeric)
xlabs = unique(dist.ass.factor)

p = ggplot(time.df, aes(dist.ass.factor, run.min.numeric)) +
  geom_boxplot(fill="lightgray") +
  scale_x_discrete(labels = xlabs) +
  scale_y_continuous(breaks=seq(0,240,30)) +
  theme_bw() +
  labs(x = "", y = "") + #, title = "Computation time in minutes") +
  coord_cartesian(ylim = c(0, 210)) # zoom in as ggg leads to max > 2700

pdf(file= paste0(getwd(),"/fig_boxplot-running-times.pdf"),
    width = 600, height = 400,
    pointsize = 12,
    bg = "white")
p
dev.off()


#### summary table of running times in minutes ---------------------------------

run.time.summary = aggregate(run.min.numeric, list(dist.ass.factor), FUN=summary) %>%
  t() %>% as.data.frame() %>% .[-1,]
colnames(run.time.summary) <- xlabs


run.time.summary


### new -------------

eval.execution_times = function(pc_list){
  
  # obtain res table
  tryCatch({
    load(paste0(pc_list$add$resultpath, "/res.RData"))
    message("res.RData successfully loaded")
  }, error = function(cond) {
    sim.merge_results(pc_list, save = T)
    load(paste0(pc_list$add$resultpath, "/res.RData"))
    print(" batches merged and loaded")
  })
  
  # select relevant variables
  time.df = res[, c("tte.dist", "prior.dist", "run.min")]
  # adjust format
  time.df$tte.dist <- as.factor(unlist(time.df$tte.dist))
  time.df$prior.dist <- as.factor(unlist(time.df$prior.dist))
  time.df$run.min = as.numeric(unlist(time.df$run.min)) 
  
  # execution time dist per tte.dist and prior.dist
  time.summaries = dplyr::summarise(
    dplyr::group_by(time.df, tte.dist, prior.dist),
    min = min(run.min, na.rm = TRUE),
    first_qu = quantile(run.min, 0.25, na.rm = TRUE),
    median = median(run.min, na.rm = TRUE),
    mean = mean(run.min, na.rm = TRUE),
    third_qu = quantile(run.min, 0.75, na.rm = TRUE),
    max = max(run.min, na.rm = TRUE),
    .groups = "drop"
  )
  
  # plot
  p = ggplot2::ggplot(time.df, ggplot2::aes(x = prior.dist, y = run.min)) +
    ggplot2::geom_boxplot(fill="lightgray") +
    ggplot2::facet_wrap(~ tte.dist) +
    ggplot2::labs(
      x = "Prior Distribution",
      y = "Run Time (min)",
      title = "Execution time in minutes") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
  
  
  return(list(summary = time.summaries, # for overview
              res.ex_times.table = time.df,       # for option to plot manually in preferred format
              plot = p
              ))
  
}


eval.execution_times(pc_list)



## END OF DOC
