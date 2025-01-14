#### auc results summary ####

nr.combined.tests = pc.aucs %>%
  select(10:ncol(.)) %>% ncol()

## boxplot of aucs for each prior choice per test ------------------------------

# boxplot over all aucs in all scenarios
pc.aucs %>%
  select(10:ncol(.)) %>%
  boxplot(main = "Test performance",
          xaxt = "n",
          xlab = "Test ID",
          ylab = "AUC")
axis(1, 1:nr.combined.tests)


# boxplot over all aucs splitted for distr. assumptions
par(mfrow = c(2,2))
pc.aucs %>%
  filter(.$dist.ass == 1) %>%
  select(10:ncol(.)) %>%
  boxplot(main = "Test performance - dist.ass = fll",
          xaxt = "n",
          xlab = "Test ID",
          ylab = "AUC")
axis(1, 1:nr.combined.tests)

pc.aucs %>%
  filter(.$dist.ass == 2) %>%
  select(10:ncol(.)) %>%
  boxplot(main = "Test performance - dist.ass = lll",
          xaxt = "n",
          xlab = "Test ID",
          ylab = "AUC")
axis(1, 1:nr.combined.tests)

pc.aucs %>%
  filter(.$dist.ass == 3) %>%
  select(10:ncol(.)) %>%
  boxplot(main = "Test performance - dist.ass = fgg",
          xaxt = "n",
          xlab = "Test ID",
          ylab = "AUC")
axis(1, 1:nr.combined.tests)

pc.aucs %>%
  filter(.$dist.ass == 4) %>%
  select(10:ncol(.)) %>%
  boxplot(main = "Test performance - dist.ass = ggg",
          xaxt = "n",
          xlab = "Test ID",
          ylab = "AUC")
axis(1, 1:nr.combined.tests)

#### mean auc results ----------------------------------------------------------
auc.means = pc.aucs %>%
  na.omit() %>%
  group_by(dist.ass) %>%
  summarize_all(mean) %>%
  select(10:ncol(.))


auc.means.vect = c(as.numeric(auc.means[1,]),
                   as.numeric(auc.means[2,]),
                   as.numeric(auc.means[3,]),
                   as.numeric(auc.means[4,]))

prior.choice = c(rep("fll",nr.combined.tests),
                 rep("lll",nr.combined.tests),
                 rep("fgg",nr.combined.tests),
                 rep("ggg",nr.combined.tests))
interval.form = c(rep("hdi",10), rep("eti", 10)) %>% rep(., 3)
cred.niveau = seq(50,95, by = 5) %>% rep(., 6)
test.option = c(rep("intuitive",20), rep("reserved",20), rep("veryreserved",20))

testnames = paste(interval.form, cred.niveau, test.option, sep = ".")
names(auc.means.vect) = testnames

auc.means.df = data.frame(prior.choice,
                          interval.form,
                          cred.niveau,
                          test.option,
                          auc.means.vect)

## ranking wrt all tests and all distributional assumptions
rank.order = order(auc.means.df$auc.means.vect, decreasing = T)

auc.means.df[rank.order,] %>% View()



#### ranking under correct prior "when"-specification ---------------------------

auc.means.0 = pc.aucs %>%
  filter(adr.ass.rank == 0) %>% # filter for correct "when"-specification
  na.omit() %>%
  group_by(dist.ass) %>%
  summarize_all(mean) %>%
  select(10:ncol(.))

auc.means.0.vect = c(as.numeric(auc.means.0[1,]),
                   as.numeric(auc.means.0[2,]),
                   as.numeric(auc.means.0[3,]),
                   as.numeric(auc.means.0[4,]))

prior.choice = c(rep("fll",60), rep("lll",60), rep("fgg",60), rep("ggg",60))
interval.form = c(rep("hdi",10), rep("eti", 10)) %>% rep(., 3)
cred.niveau = seq(50,95, by = 5) %>% rep(., 6)
test.option = c(rep("intuitive",20), rep("reserved",20), rep("veryreserved",20))
testnames = paste(prior.choice, interval.form, cred.niveau, test.option, sep = ".")
names(auc.means.0.vect) = testnames

auc.means.0.df = data.frame(prior.choice, interval.form, cred.niveau, test.option, auc.means.0.vect)

## ranking wrt all tests and all distributional assumptions
rank.order.0 = order(auc.means.0.df$auc.means.0.vect, decreasing = T)

auc.means.0.df[rank.order.0,] %>% View()


## ranking grouped by distributional assumption for all dists.

auc.means.dist.0 = list()

auc.means.dist.0[[1]] = auc.means.0.df[rank.order.0,] %>%
  filter(prior.choice =="fll")

auc.means.dist.0[[2]] = auc.means.0.df[rank.order.0,] %>%
  filter(prior.choice =="lll")

auc.means.dist.0[[3]] = auc.means.0.df[rank.order.0,] %>%
  filter(prior.choice =="fgg")

auc.means.dist.0[[4]] = auc.means.0.df[rank.order.0,] %>%
  filter(prior.choice =="ggg")

# latex tables:
dist = c("fll", "lll", "fgg", "ggg")

for(i in 1:4){

  thecaption = "yet to be filled in"

  print(xtable(auc.means.dist.0[[i]],
             digits = c(rep(0,5),3), # first zero "represents" row numbers which we skip later
             align = "r|llrlr|",  # align and put a vertical line (first "l" again represents column of row numbers)
             caption = thecaption,
             label = paste0("tab:auc_ranking_", dist[i])),
        size = "footnotesize", #Change size; useful for bigger tables "normalsize" "footnotesize"
        include.rownames = FALSE, #Don't print rownames
        include.colnames = T,
        caption.placement = "bottom", #"top","bottom", NULL
        hline.after=c(-1,0,nrow(auc.means.dist.0[[i]])),
        floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
        sanitize.text.function = force, # Important to treat content as latex function
        table.placement="h"
  )

}


## END OF DOC
