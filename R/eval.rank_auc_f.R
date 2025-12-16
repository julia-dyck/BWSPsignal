#'
#' @noRd

## fct for frequentist tests

eval.rank_auc_f = function(perf_f){
  require(dplyr)
  
  # 0. argument check
  required_cols <- c(
    "N","br","adr.rate","adr.when","adr.relsd","study.period",
    "tte.dist","cred.level",
    "auc","fpr","tpr","fnr","tnr"
  )
  
  num_cols <- c(
    "N","br","adr.rate","adr.when","adr.relsd","study.period",
    "cred.level","auc","fpr","tpr","fnr","tnr"
  )
  
  chr_cols <- c("tte.dist")
  
  if (
    !is.data.frame(perf_f) ||
    !all(required_cols %in% names(perf_f)) ||
    !all(vapply(perf_f[num_cols], is.numeric, logical(1))) ||
    !all(vapply(perf_f[chr_cols], is.character, logical(1)))
  ) {
    stop("Argument `perf_f` has wrong format. It must be the output of eval.calc_perf_f().")
  }
  
  
  tab.df = perf_f
  
  # 1. summarize and rank fit & test specifications
  
  # ranking
  tab.ranked <- tab.df %>%
    dplyr::group_by(tte.dist, cred.level) %>% # group by fit&test specifications
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
                     .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(AUC)) # ranking
  
  
  # 2. filter for best fit & test specification and investigate effect of scenario parameters 
  
  opti.tte.dist = tab.ranked$tte.dist[1]
  opti.cred.level = tab.ranked$cred.level[1]
  
  # optimal fit & test specification according to ranking under correct prior belief
  tab.opti = tab.df %>% filter(tte.dist == opti.tte.dist, 
                               cred.level == opti.cred.level,
  )
  
  # Effect of N
  tab.opti.N = tab.opti %>% 
    group_by(N) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
                     .groups = "drop")
  
  # Effect of br
  tab.opti.br = tab.opti %>% 
    group_by(br) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop")
  
  # Effect of adr.rate
  tab.opti.adr.rate = tab.opti %>% 
    group_by(adr.rate) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
                     .groups = "drop")
  
  # Effect of adr.when
  tab.opti.adr.when = tab.opti %>% 
    group_by(adr.when) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
                     .groups = "drop")
  
  # Effect of adr.relsd
  tab.opti.adr.relsd = tab.opti %>% 
    group_by(adr.relsd) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
                     .groups = "drop")
  
  # Output 
  out = list(
    ranking = tab.ranked,
    effect.of.N = tab.opti.N,
    effect.of.br = tab.opti.br,
    effect.of.adr.rate = tab.opti.adr.rate,
    effect.of.adr.when = tab.opti.adr.when,
    effect.of.adr.relsd = tab.opti.adr.relsd
  )
  return(out)
  
}


## END OF DOC


