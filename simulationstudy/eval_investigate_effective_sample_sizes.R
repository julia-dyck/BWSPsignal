#### evaluation of simstudy01 - investigate the effective sample sizes ####

# initiated 4x10 000 iterations in the NUTS sampling (without burn in)
# goal: effective sample size of \approx 10 000 for nu and gamma parameter


dist.ass.factor = res$dist.ass %>% as.numeric() %>% as.factor()

## for nu ----------------------------------------------------------------------
nu.n_eff.numeric = res$nu.po.n_eff %>% as.numeric()

aggregate(nu.n_eff.numeric, list(dist.ass.factor), FUN=summary) %>%
  t()

aggregate(nu.n_eff.numeric > 10000, list(dist.ass.factor), FUN=table) %>%
  t()

## for gamma -------------------------------------------------------------------
ga.n_eff.numeric = res$ga.po.n_eff %>% as.numeric()

aggregate(ga.n_eff.numeric, list(dist.ass.factor), FUN=summary) %>%
  t()

aggregate(nu.n_eff.numeric > 10000, list(dist.ass.factor), FUN=table) %>%
  t()

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

png(file = paste0(wd_output, "/graphics/fig_boxplot-effective-sample-sizes.png"),
    width = 600, height = 400,
    units = "px", pointsize = 12)

dev.off()

## END OF DOC
