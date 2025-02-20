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

## END OF DOC
