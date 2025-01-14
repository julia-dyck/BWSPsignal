#### auc lll results - summary ####

#### lll and correct prior specification ---------------------------------------

pc.aucs.lll = pc.aucs %>%
  filter(dist.ass == 2)


## boxplot over all aucs in all lll scenarios
par(mfrow = c(1,1))
pc.aucs.lll %>%
  filter(adr.ass.rank == 0) %>%
  select(10:ncol(.)) %>%
  boxplot(sub = "lll; correct prior 'when'- spec.",
          main = "Test performance",
          horizontal = T, yaxt = "n",
          ylab = "Test ID",
          xlab = "AUC")
axis(2, las = 2)


## mean auc results ------------------------------------------------------------

auc.means.lll.0 = pc.aucs.lll %>%
  filter(adr.ass.rank == 0) %>%
  summarize_all(mean) %>%
  select(10:ncol(.)) %>%
  as.numeric(.)

# add test setup components to table
prior.choice = rep("lll",60)
interval.form = c(rep("hdi",10), rep("eti", 10)) %>% rep(., 3)
cred.niveau = seq(50,95, by = 5) %>% rep(., 6)
test.option = c(rep("intuitive",20), rep("reserved",20), rep("veryreserved",20))
testnames = paste(prior.choice, interval.form, cred.niveau, test.option, sep = ".")
names(auc.means.lll.0) = testnames

auc.means.lll.0.df = data.frame(prior.choice, interval.form, cred.niveau, test.option, auc.means.lll.0)

# View(auc.means.lll.0.df)


# AUC ranking to see which test combination works relatively best --------------

rank.order.lll.0 = pc.aucs.lll %>%
  filter(adr.ass.rank == 0) %>%
  summarize_all(mean) %>%
  select(10:ncol(.)) %>%
  as.numeric() %>%
  order(decreasing = T)

auc.means.lll.0.df[rank.order.lll.0,] %>% View() # ranking table

# best test in first row of ranking table:
colnames(pc.aucs)[9 + 27] # "hdi.80.reserved"

#### auc of best test under correct prior 'when' - specification ---------------

# overall mean for best test
overall = pc.aucs.lll %>%
  filter(adr.ass.rank == 0) %>%
  summarize_all(mean) %>%
  select(., hdi.80.reserved)

# grouped by N
by.N = pc.aucs.lll %>%
  filter(adr.ass.rank == 0) %>%
  group_by(N) %>%
  summarize_all(mean) %>%
  select(.,N, hdi.80.reserved)

# grouped by adr.rate
by.adr.rate = pc.aucs.lll %>%
  filter(adr.ass.rank == 0) %>%
  group_by(adr.rate) %>%
  summarize_all(mean) %>%
  select(.,adr.rate, hdi.80.reserved)

# grouped by adr.when
by.adr.when = pc.aucs.lll %>%
  filter(adr.ass.rank == 0) %>%
  group_by(adr.when) %>%
  summarize_all(mean) %>%
  select(.,adr.when, hdi.80.reserved)

# table for latex
grouping.factors = c("none", "N", "", "", "proportion of ADR events", "", "expected time of ADR events", "", "")
group.name = c("overall",
               "$500$", "$3000$", "$5000$",
               "$5\\%$ of $N$", "$10\\%$ of $N$",
               "1st quarter of observation period",
               "2nd quarter of observation period",
               "3rd quarter of observation period")

grouped.aucs = c(pull(overall, hdi.80.reserved),
                      pull(by.N, hdi.80.reserved),
                      pull(by.adr.rate, hdi.80.reserved),
                      pull(by.adr.when, hdi.80.reserved)
                      )


best.test.table  = data.frame(grouping.factors, group = group.name, AUC = grouped.aucs)
best.test.table

thecaption = "Average AUC performance of signal detection test (prior distribution: log-log-log, ROPE: $80\\%$ Lognormal confidence interval, credibility interval: $80\\%$ highest posterior density region, test option: reserved) grouped by sample size, proportion of ADR events, and expected time of ADR events."

print(xtable(best.test.table, digits = c(0,0,0,3), # first zero "represents" row numbers which we skip later
             align = "llrr",  # align and put a vertical line (first "l" again represents column of row numbers)
             caption = thecaption,
             label = "tab:hdi.80.reserved"),
      size = "normalsize", #Change size; useful for bigger tables "normalsize" "footnotesize"
      include.rownames = FALSE, #Don't print rownames
      include.colnames = T,
      caption.placement = "bottom", #"top","bottom", NULL
      hline.after=c(-1,1,4,6,9),
      floating=TRUE, # whether \begin{Table} should be created (TRUE) or not (FALSE)
      sanitize.text.function = force, # eg. to treat content as latex function
      table.placement="t"
)




#### lll and robustness wrt to prior specification -----------------------------

# grouped by adr.ass.rank
# rank definition:
# ## 0 = correct prior ass
# ## 1 = one quarter of observation time frame off
# ## 2 = two quarter of observation time frame off
# ## 3 = "no adr" as prior assumption (which is true for control)

pc.aucs.lll %>%
  group_by(adr.ass.rank) %>%
  summarize_all(mean) %>%
  select(.,adr.ass.rank, hdi.80.reserved)


## END OF DOCUMENT
