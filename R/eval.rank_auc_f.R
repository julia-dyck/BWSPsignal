#' Evaluation: rank FWSP tests by AUC
#'
#' Ranks all tried fit & test specifications by AUC.
#'
#' @param perf_f A data frame containing Bayesian performance metric results returned by \link{eval.calc_perf_f}.
#'
#'
#' @return A list containing 
#' \itemize{
#' \item \code{$ranking}: Ranking of fit and FWSP test specifications according 
#' to AUC averaged over all sample scenarios.
#' \item \code{$effect.of.N}: Effect of sample size on AUC for the optimal fit 
#' and FWSP test specification.
#' \item \code{$effect.of.br}: Effect of background rate on AUC for the optimal fit
#' and FWSP test specification.
#' \item \code{$effect.of.adr.rate}: Effect of ADR rate on AUC for the optimal fit
#' and FWSP test specification.
#' \item \code{$effect.of.adr.when}: Effect of true expected event times on AUC 
#' for the optimal fit and FWSP test specification.
#' \item \code{$effect.of.adr.relsd}: Effect of relative standard deviation of event 
#' time on AUC for the optimal fit and FWSP test specification.
#' }
#' 
#' 
#' @export


eval.rank_auc_f = function(perf_f){
  require(dplyr)
  
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
    group_by(N) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of br
  tab.opti.br = tab.opti %>% 
    group_by(br) %>% 
    summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
              .groups = "drop")
  
  # Effect of adr.rate
  tab.opti.adr.rate = tab.opti %>% 
   group_by(adr.rate) %>% 
    summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
              .groups = "drop")
  
  # Effect of adr.when
  tab.opti.adr.when = tab.opti %>% 
    group_by(adr.when) %>% 
    summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
              .groups = "drop")
  
  # Effect of adr.relsd
  tab.opti.adr.relsd = tab.opti %>% 
    group_by(adr.relsd) %>% 
    summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr),
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

