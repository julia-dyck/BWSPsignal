#### evaluate execution times for different prior distributional choices


# calculate execution times for different dist. assumptions
dist.ass.factor = res$dist.ass %>% as.numeric() %>% as.factor()

run.min.numeric = res$run.min %>% as.numeric() # in minutes
run.h.numeric = run.min.numeric/60             # in hours


#### boxplot of execution times ------------------------------------------------

time.df = data.frame(dist.ass.factor, run.min.numeric)
xlabs = c("fix-log-log", "log-log-log", "fix-gam-gam", "gam-gam-gam")

p = ggplot(time.df, aes(dist.ass.factor, run.min.numeric)) +
  geom_boxplot(fill="lightgray") +
  scale_x_discrete(labels = xlabs) +
  scale_y_continuous(breaks=seq(0,240,30)) +
  theme_bw() +
  labs(x = "", y = "") + #, title = "Computation time in minutes") +
  coord_cartesian(ylim = c(0, 210)) # zoom in as ggg leads to max > 2700

png(file= paste0(getwd(),"/fig_boxplot-running-times.png"),
    width = 600, height = 400,
    units = "px", pointsize = 12,
    bg = "white")
p
dev.off()


#### summary table of running times in minutes ---------------------------------

run.time.summary = aggregate(run.min.numeric, list(dist.ass.factor), FUN=summary) %>%
  t() %>% as.data.frame() %>% .[-1,]
colnames(run.time.summary) <- xlabs


run.time.summary


## END OF DOC
