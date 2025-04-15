#### investigation of the effective sample sizes 

# Note:
# posterior sample size in each estimation: 10 000 iterations (without burn in)
# per chain & 4 chains -> 40 000 iterations in total

# threshold value for effective sample size (ESS) (acc. to Kruschke, 2015)
# \approx 10 000 for each shape parameter 

#### summary of ESS per prior distributional choice ----------------------------

dist.ass.factor = res$dist.ass %>% as.numeric() %>% as.factor()

## for shape parameter nu 
nu.n_eff.numeric = res$nu.po.n_eff %>% as.numeric()

nu.n_eff_summary = aggregate(nu.n_eff.numeric, list(dist.ass.factor), FUN=summary) %>%
  t()
colnames(nu.n_eff_summary) = unique(pc$dist.ass)
nu.n_eff_summary # summary of ESS per dist. ass. over all simulation runs

nu.n_eff_thr = aggregate(nu.n_eff.numeric > 10000, list(dist.ass.factor), FUN=table) %>%
  t()
colnames(nu.n_eff_thr) = unique(pc$dist.ass)
nu.n_eff_thr # no of simulation runs exceeding the recommended ESS of 10 000 
             # (stratified wrt prior distributional choice).


## for shape parameter gamma 
ga.n_eff.numeric = res$ga.po.n_eff %>% as.numeric()

ga.n_eff_summary = aggregate(ga.n_eff.numeric, list(dist.ass.factor), FUN=summary) %>%
  t() 
colnames(ga.n_eff_summary) = unique(pc$dist.ass)
ga.n_eff_summary # summary of ESS per dist. ass. over all simulation runs

ga.n_eff_thr = aggregate(nu.n_eff.numeric > 10000, list(dist.ass.factor), FUN=table) %>%
  t()
colnames(ga.n_eff_thr) = unique(pc$dist.ass)
ga.n_eff_thr # no of simulation runs exceeding the recommended ESS of 10 000 
             # (stratified wrt prior distributional choice).


#### boxplot of effective sample sizes -----------------------------------------

n_eff.df = data.frame(dist.ass = rep(dist.ass.factor,2),
                      n_eff = c(nu.n_eff.numeric, ga.n_eff.numeric),
                      parameter = c(rep("nu", length(nu.n_eff.numeric)),
                        rep("gamma", length(ga.n_eff.numeric))))

p = ggplot(n_eff.df, aes(dist.ass, n_eff, fill = parameter)) +
  geom_boxplot() +
  scale_x_discrete(labels = xlabs) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "") +
  geom_hline(yintercept = 10000, linetype="dashed",
           color = "black", linewidth = 1)

p

pdf(file = paste0(getwd(), "/fig_boxplot-effective-sample-sizes.pdf"),
    width = 600, height = 400,
    pointsize = 12)

dev.off()


## new as fct


eval.eff_sample_sizes = function(pc_list, threshold = 10000){
  
  if (!exists("res")) { 
    # obtain res table
    tryCatch({
      load(paste0(pc_list$add$resultpath, "/res.RData"))
      message("res.RData successfully loaded")
    }, error = function(cond) {
      sim.merge_results(pc_list, save = T)
      load(paste0(pc_list$add$resultpath, "/res.RData"))
      print(" batches merged and loaded")
    })
  }
  else{
    message("Object `res` loaded in current environment is used to extract execution times.")
  }
  
  # select relevant variables
  n_eff.df = res[, c("tte.dist", "prior.dist", "nu.po.n_eff", "ga.po.n_eff")]
  # adjust format
  n_eff.df$tte.dist <- as.factor(unlist(n_eff.df$tte.dist))
  n_eff.df$prior.dist <- as.factor(unlist(n_eff.df$prior.dist))
  n_eff.df$nu.po.n_eff = as.numeric(unlist(n_eff.df$nu.po.n_eff)) 
  n_eff.df$ga.po.n_eff = as.numeric(unlist(n_eff.df$ga.po.n_eff)) 
  
  # execution time dist per tte.dist and prior.dist
  ## WHAT DO WE REALLY NEED; MEAN; NO OF SIMS WITH NEFF < 10000; LOOK AT MANUSCRIPT
  n_eff.summaries = dplyr::summarise(
    dplyr::group_by(n_eff.df, tte.dist, prior.dist),
    # statistics on n_eff for shape 1
    shape1.min = min(nu.po.n_eff, na.rm = TRUE),
    shape1.first_qu = quantile(nu.po.n_eff, 0.25, na.rm = TRUE),
    shape1.median = median(nu.po.n_eff, na.rm = TRUE),
    shape1.mean = mean(nu.po.n_eff, na.rm = TRUE),
    shape1.third_qu = quantile(nu.po.n_eff, 0.75, na.rm = TRUE),
    shape1.max = max(nu.po.n_eff, na.rm = TRUE),
    shape1.n_eff_above_thr = table(nu.po.n_eff > threshold)[2] / sum(table(nu.po.n_eff > threshold)),
    # statistics on n_eff for shape 2
    shape2.min = min(ga.po.n_eff, na.rm = TRUE),
    shape2.first_qu = quantile(ga.po.n_eff, 0.25, na.rm = TRUE),
    shape2.median = median(ga.po.n_eff, na.rm = TRUE),
    shape2.mean = mean(ga.po.n_eff, na.rm = TRUE),
    shape2.third_qu = quantile(ga.po.n_eff, 0.75, na.rm = TRUE),
    shape2.max = max(ga.po.n_eff, na.rm = TRUE),
    shape2.n_eff_above_thr = table(ga.po.n_eff > threshold)[2] / sum(table(ga.po.n_eff > threshold)),
    
    .groups = "drop"
  )
  return(n_eff.summaries)
  
  # plot ## SUBSTITUTE RUN:MIN BY nu.po.n_eff & ga.po.n_eff
  p = ggplot2::ggplot(n_eff.df, ggplot2::aes(x = prior.dist, y = run.min, fill= tte.dist)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~ tte.dist) +
    ggplot2::labs(
      x = "Prior Distribution",
      y = "Run Time (min)",
      title = "Execution time in minutes") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")
  
  return(list(summary = time.summaries,     # for overview
              plot = p,                     # for option to take plot as is and manipulate if further
              res.n_eff.table = n_eff.df  # for option to plot manually in preferred format
  ))
}



n_eff.out = eval.eff_sample_sizes(pc_list, threshold = 10000)
View(n_eff.out)




## END OF DOC
