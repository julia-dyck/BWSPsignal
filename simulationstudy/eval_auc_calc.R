#### AUC calculation per scenario

# REMARK: adjust setup of ROPES and HDIs 

#### perform HDI+ROPE tests ----------------------------------------------------

# prior parameters under null hypothesis
logpars1 = logprior_repar(m_t = 1,s_t = 10)

# probability mass and percentiles for rope intervals
prob = seq(0.5, 0.95, by = 0.05)
rope.percentile.l = (1-prob)/2
rope.percentile.u = prob + (1-prob)/2
rope.perc = cbind(rope.percentile.l, rope.percentile.u)
rope.perc

## ROPEs
# Lognormal confidence intervals under null-hypthesis as ropes
ropes = apply(X = rope.perc, MARGIN = 1, FUN = qlnorm, meanlog =logpars1[1], sdlog = logpars1[2]) %>%
  t() %>%
  rbind(.,.,.) %>%
  rbind(.,.)
ropes

## HDIs (and ETIs respectively)
# posterior credibility regions will be extracted from sim results, e.g.
# nu.hdi0.5l and nu.hdi0.5u as lower & upper boundary for 50% - highest density 
# region of nu


# Have a look at the following table for definition of all HDI+ROPE test variants
# tried out:
hdi_rope_test_variants = read.csv("test_combis_predx.csv", 
                                  header = TRUE, row.names = NULL, sep = ";")
hdi_rope_test_variants

## perform test no [x] for each simulation repetition and store in col "pred[x]"
res.pred = res %>%
  mutate(pred1 = apply(.[,c("nu.hdi0.5l","nu.hdi0.5u", "ga.hdi0.5l", "ga.hdi0.5u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[1,], option = 1, mod = "pgw"),
         pred2 = apply(.[,c("nu.hdi0.55l","nu.hdi0.55u", "ga.hdi0.55l", "ga.hdi0.55u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[2,], option = 1, mod = "pgw"),
         pred3 = apply(.[,c("nu.hdi0.6l","nu.hdi0.6u", "ga.hdi0.6l", "ga.hdi0.6u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[3,], option = 1, mod = "pgw"),
         pred4 = apply(.[,c("nu.hdi0.65l","nu.hdi0.65u", "ga.hdi0.65l", "ga.hdi0.65u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[4,], option = 1, mod = "pgw"),
         pred5 = apply(.[,c("nu.hdi0.7l","nu.hdi0.7u", "ga.hdi0.7l", "ga.hdi0.7u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[5,], option = 1, mod = "pgw"),
         pred6 = apply(.[,c("nu.hdi0.75l","nu.hdi0.75u", "ga.hdi0.75l", "ga.hdi0.75u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[6,], option = 1, mod = "pgw"),
         pred7 = apply(.[,c("nu.hdi0.8l","nu.hdi0.8u", "ga.hdi0.8l", "ga.hdi0.8u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[7,], option = 1, mod = "pgw"),
         pred8 = apply(.[,c("nu.hdi0.85l","nu.hdi0.85u", "ga.hdi0.85l", "ga.hdi0.85u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[8,], option = 1, mod = "pgw"),
         pred9 = apply(.[,c("nu.hdi0.9l","nu.hdi0.9u", "ga.hdi0.9l", "ga.hdi0.9u")],
                       MARGIN = 1,
                       FUN = bwsp_test,
                       nullregion = ropes[9,], option = 1, mod = "pgw"),
         pred10 = apply(.[,c("nu.hdi0.95l","nu.hdi0.95u", "ga.hdi0.95l", "ga.hdi0.95u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[10,], option = 1, mod = "pgw"),
         pred11 = apply(.[,c("nu.eti0.5l","nu.eti0.5u", "ga.eti0.5l", "ga.eti0.5u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[11,], option = 1, mod = "pgw"),
         pred12 = apply(.[,c("nu.eti0.55l","nu.eti0.55u", "ga.eti0.55l", "ga.eti0.55u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[12,], option = 1, mod = "pgw"),
         pred13 = apply(.[,c("nu.eti0.6l","nu.eti0.6u", "ga.eti0.6l", "ga.eti0.6u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[13,], option = 1, mod = "pgw"),
         pred14 = apply(.[,c("nu.eti0.65l","nu.eti0.65u", "ga.eti0.65l", "ga.eti0.65u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[14,], option = 1, mod = "pgw"),
         pred15 = apply(.[,c("nu.eti0.7l","nu.eti0.7u", "ga.eti0.7l", "ga.eti0.7u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[15,], option = 1, mod = "pgw"),
         pred16 = apply(.[,c("nu.eti0.75l","nu.eti0.75u", "ga.eti0.75l", "ga.eti0.75u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[16,], option = 1, mod = "pgw"),
         pred17 = apply(.[,c("nu.eti0.8l","nu.eti0.8u", "ga.eti0.8l", "ga.eti0.8u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[17,], option = 1, mod = "pgw"),
         pred18 = apply(.[,c("nu.eti0.85l","nu.eti0.85u", "ga.eti0.85l", "ga.eti0.85u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[18,], option = 1, mod = "pgw"),
         pred19 = apply(.[,c("nu.eti0.9l","nu.eti0.9u", "ga.eti0.9l", "ga.eti0.9u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[19,], option = 1, mod = "pgw"),
         pred20 = apply(.[,c("nu.eti0.95l","nu.eti0.95u", "ga.eti0.95l", "ga.eti0.95u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[20,], option = 2, mod = "pgw"),
         pred21 = apply(.[,c("nu.hdi0.5l","nu.hdi0.5u", "ga.hdi0.5l", "ga.hdi0.5u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[21,], option = 2, mod = "pgw"),
         pred22 = apply(.[,c("nu.hdi0.55l","nu.hdi0.55u", "ga.hdi0.55l", "ga.hdi0.55u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[22,], option = 2, mod = "pgw"),
         pred23 = apply(.[,c("nu.hdi0.6l","nu.hdi0.6u", "ga.hdi0.6l", "ga.hdi0.6u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[23,], option = 2, mod = "pgw"),
         pred24 = apply(.[,c("nu.hdi0.65l","nu.hdi0.65u", "ga.hdi0.65l", "ga.hdi0.65u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[24,], option = 2, mod = "pgw"),
         pred25 = apply(.[,c("nu.hdi0.7l","nu.hdi0.7u", "ga.hdi0.7l", "ga.hdi0.7u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[25,], option = 2, mod = "pgw"),
         pred26 = apply(.[,c("nu.hdi0.75l","nu.hdi0.75u", "ga.hdi0.75l", "ga.hdi0.75u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[26,], option = 2, mod = "pgw"),
         pred27 = apply(.[,c("nu.hdi0.8l","nu.hdi0.8u", "ga.hdi0.8l", "ga.hdi0.8u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[27,], option = 2, mod = "pgw"),
         pred28 = apply(.[,c("nu.hdi0.85l","nu.hdi0.85u", "ga.hdi0.85l", "ga.hdi0.85u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[28,], option = 2, mod = "pgw"),
         pred29 = apply(.[,c("nu.hdi0.9l","nu.hdi0.9u", "ga.hdi0.9l", "ga.hdi0.9u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[29,], option = 2, mod = "pgw"),
         pred30 = apply(.[,c("nu.hdi0.95l","nu.hdi0.95u", "ga.hdi0.95l", "ga.hdi0.95u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[30,], option = 2, mod = "pgw"),
         pred31 = apply(.[,c("nu.eti0.5l","nu.eti0.5u", "ga.eti0.5l", "ga.eti0.5u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[31,], option = 2, mod = "pgw"),
         pred32 = apply(.[,c("nu.eti0.55l","nu.eti0.55u", "ga.eti0.55l", "ga.eti0.55u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[32,], option = 2, mod = "pgw"),
         pred33 = apply(.[,c("nu.eti0.6l","nu.eti0.6u", "ga.eti0.6l", "ga.eti0.6u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[33,], option = 2, mod = "pgw"),
         pred34 = apply(.[,c("nu.eti0.65l","nu.eti0.65u", "ga.eti0.65l", "ga.eti0.65u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[34,], option = 2, mod = "pgw"),
         pred35 = apply(.[,c("nu.eti0.7l","nu.eti0.7u", "ga.eti0.7l", "ga.eti0.7u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[35,], option = 2, mod = "pgw"),
         pred36 = apply(.[,c("nu.eti0.75l","nu.eti0.75u", "ga.eti0.75l", "ga.eti0.75u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[36,], option = 2, mod = "pgw"),
         pred37 = apply(.[,c("nu.eti0.8l","nu.eti0.8u", "ga.eti0.8l", "ga.eti0.8u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[37,], option = 2, mod = "pgw"),
         pred38 = apply(.[,c("nu.eti0.85l","nu.eti0.85u", "ga.eti0.85l", "ga.eti0.85u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[38,], option = 2, mod = "pgw"),
         pred39 = apply(.[,c("nu.eti0.9l","nu.eti0.9u", "ga.eti0.9l", "ga.eti0.9u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[39,], option = 2, mod = "pgw"),
         pred40 = apply(.[,c("nu.eti0.95l","nu.eti0.95u", "ga.eti0.95l", "ga.eti0.95u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[40,], option = 3, mod = "pgw"),
         pred41 = apply(.[,c("nu.hdi0.5l","nu.hdi0.5u", "ga.hdi0.5l", "ga.hdi0.5u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[41,], option = 3, mod = "pgw"),
         pred42 = apply(.[,c("nu.hdi0.55l","nu.hdi0.55u", "ga.hdi0.55l", "ga.hdi0.55u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[42,], option = 3, mod = "pgw"),
         pred43 = apply(.[,c("nu.hdi0.6l","nu.hdi0.6u", "ga.hdi0.6l", "ga.hdi0.6u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[43,], option = 3, mod = "pgw"),
         pred44 = apply(.[,c("nu.hdi0.65l","nu.hdi0.65u", "ga.hdi0.65l", "ga.hdi0.65u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[44,], option = 3, mod = "pgw"),
         pred45 = apply(.[,c("nu.hdi0.7l","nu.hdi0.7u", "ga.hdi0.7l", "ga.hdi0.7u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[45,], option = 3, mod = "pgw"),
         pred46 = apply(.[,c("nu.hdi0.75l","nu.hdi0.75u", "ga.hdi0.75l", "ga.hdi0.75u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[46,], option = 3, mod = "pgw"),
         pred47 = apply(.[,c("nu.hdi0.8l","nu.hdi0.8u", "ga.hdi0.8l", "ga.hdi0.8u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[47,], option = 3, mod = "pgw"),
         pred48 = apply(.[,c("nu.hdi0.85l","nu.hdi0.85u", "ga.hdi0.85l", "ga.hdi0.85u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[48,], option = 3, mod = "pgw"),
         pred49 = apply(.[,c("nu.hdi0.9l","nu.hdi0.9u", "ga.hdi0.9l", "ga.hdi0.9u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[49,], option = 3, mod = "pgw"),
         pred50 = apply(.[,c("nu.hdi0.95l","nu.hdi0.95u", "ga.hdi0.95l", "ga.hdi0.95u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[50,], option = 3, mod = "pgw"),
         pred51 = apply(.[,c("nu.eti0.5l","nu.eti0.5u", "ga.eti0.5l", "ga.eti0.5u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[51,], option = 3, mod = "pgw"),
         pred52 = apply(.[,c("nu.eti0.55l","nu.eti0.55u", "ga.eti0.55l", "ga.eti0.55u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[52,], option = 3, mod = "pgw"),
         pred53 = apply(.[,c("nu.eti0.6l","nu.eti0.6u", "ga.eti0.6l", "ga.eti0.6u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[53,], option = 3, mod = "pgw"),
         pred54 = apply(.[,c("nu.eti0.65l","nu.eti0.65u", "ga.eti0.65l", "ga.eti0.65u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[54,], option = 3, mod = "pgw"),
         pred55 = apply(.[,c("nu.eti0.7l","nu.eti0.7u", "ga.eti0.7l", "ga.eti0.7u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[55,], option = 3, mod = "pgw"),
         pred56 = apply(.[,c("nu.eti0.75l","nu.eti0.75u", "ga.eti0.75l", "ga.eti0.75u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[56,], option = 3, mod = "pgw"),
         pred57 = apply(.[,c("nu.eti0.8l","nu.eti0.8u", "ga.eti0.8l", "ga.eti0.8u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[57,], option = 3, mod = "pgw"),
         pred58 = apply(.[,c("nu.eti0.85l","nu.eti0.85u", "ga.eti0.85l", "ga.eti0.85u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[58,], option = 3, mod = "pgw"),
         pred59 = apply(.[,c("nu.eti0.9l","nu.eti0.9u", "ga.eti0.9l", "ga.eti0.9u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[59,], option = 3, mod = "pgw"),
         pred60 = apply(.[,c("nu.eti0.95l","nu.eti0.95u", "ga.eti0.95l", "ga.eti0.95u")],
                        MARGIN = 1,
                        FUN = bwsp_test,
                        nullregion = ropes[60,], option = 3, mod = "pgw")
  )

nr.combined.tests = ncol(res.pred) - ncol(res)
nr.combined.tests


#### setup pc.pos in data.frame format for auc results per scenario ------------

## control cases are not in there, as they are assigned to each ADR-positive
# scenario (pc.pos) when calculating the AUC
pc.pos = pc.num %>%
  filter(.$label == 1) %>%
  mutate(adr.ass.tr = adr.ass/4) %>%
  mutate(adr.ass.tr2 = ifelse(adr.ass == 4, 9, adr.ass.tr)) %>%
  mutate(adr.ass.rank0 = abs(adr.when - adr.ass.tr2)*4) %>%
  mutate(adr.ass.rank = ifelse(adr.ass.rank0 > 10, 3, adr.ass.rank0)) %>%
  select(., -c(adr.ass.tr, adr.ass.tr2, adr.ass.rank0))
# adr.ass.rank variable definition:
# ## 0 = correct prior ass
# ## 1 = one quarter of observation time frame off
# ## 2 = two quarter of observation time frame off
# ## 3 = "no adr" as prior assumption (which is true for control)


# matrix for AUC results -----------------------------------------------

aucs = matrix(rep(0, nr.combined.tests*nrow(pc.pos)), ncol = nr.combined.tests)
colnames(aucs) = paste0("auc", 1:nr.combined.tests)

dims.restest = c()
# go through every ADR-positive scenario linked with control
for(i in 1:nrow(pc.pos)){
  N_i = pc.pos$N[i]
  adr.rate_i = pc.pos$adr.rate[i]
  adr.when_i = pc.pos$adr.when[i]
  dist.ass_i = pc.pos$dist.ass[i]
  adr.ass_i = pc.pos$adr.ass[i]

  res.test = res.pred %>%
    filter(.$adr.rate %in% c(0, adr.rate_i),
           .$adr.when %in% c(0, adr.when_i),
           .$N == N_i,
           .$dist.ass == dist.ass_i,
           .$adr.ass == adr.ass_i)
  dims.restest[i] = dim(res.test)[1]
  if(dims.restest[i]==200){
    # set up labels and predictions in a matrix
    labels = res.test$lab
    for(testnr in 2:nr.combined.tests){
      labels = cbind(labels, res.test$lab)
    }
    predictions = data.frame(res.test)[,314:(313 + nr.combined.tests)] %>%
      as.matrix()

    # creating prediction object
    pred.obj <- ROCR::prediction(predictions, labels)
    # dann noch AUC berechnen
    aucs[i,] = ROCR::performance(pred.obj, "auc") %>%
      .@y.values %>%
      as.numeric()
    # later, if still of interest
    #rocs = ROCR::performance(pred.obj, "tpr", "fpr") %>%
    ###  plot(lwd = 2)
  }
  else{
    aucs[i,] = rep(NA, nr.combined.tests)
  }

}


#### merge pc specification and auc results ------------------------------------
pc.aucs = data.frame(pc.pos, aucs) %>%
  select(., -label)

interval.form = c(rep("hdi",10), rep("eti", 10)) %>% rep(., 3)
cred.niveau = seq(50,95, by = 5) %>% rep(., 6)
test.option = c(rep("intuitive",20), rep("reserved",20), rep("veryreserved",20))
testnames = paste(interval.form, cred.niveau, test.option, sep = ".")

colnames(aucs) = paste(testnames, sep = ".")
colnames(pc.aucs)[-(1:9)] = paste(testnames, sep = ".")

View(pc.aucs)
## END OF DOC
