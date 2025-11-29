#' Rank WSP test configurations by AUC obtained from simulation
#'
#' Ranks all fit (defined by time-to-event and prior distribution) 
#' and test specifications grouped by simulation scenarios in terms
#' of the corresponding area under the curve (AUC) value for either Bayesian or 
#' frequentist Weibull Shape Parameter (BWSP, FWSP) tests.
#'
#' @param perf_b data frame containing performance results for BWSP tests 
#'               returned by \link{eval.calc_perf_b}
#' @param perf_f data frame containing performance results for FWSP tests 
#'               returned by \link{eval.calc_perf_f} 
#'
#' @return A list containing 
#' \itemize{
#' \item \code{$ranking}: Ranking of fit and WSP test specifications according 
#' to AUC averaged over all sample scenarios (- for BWSP given a correct 
#' specification of prior belief)
#' \item \code{$effect.of.N}: Effect of sample size on AUC for the optimal fit 
#' and WSP test (- for BWSP given a correct specification of prior belief)
#' \item \code{$effect.of.br}: Effect of background rate on AUC for the optimal fit
#' and WSP test (- for BWSP given a correct specification of prior belief)
#' \item \code{$effect.of.adr.rate}: Effect of ADR rate on AUC for the optimal fit
#' and WSP test (- for BWSP given a correct specification of prior belief)
#' \item \code{$effect.of.adr.when}: Effect of true expected event times on AUC 
#' for the optimal fit and WSP test (- for BWSP given a correct 
#' specification of prior belief)
#' \item \code{$effect.of.adr.relsd}: Effect of relative standard deviation of event 
#' time on AUC for the optimal fit and WSP test (- for BWSP given a correct 
#' specification of prior belief)
#' \item \code{$effect.of.dist.prior.to.truth}: Effect of distance of prior belief 
#' to true adr.when on AUC for the optimal fit and WSP test
#' }
#' 
#' @details For definitions of the performance metrics returned in output, 
#' see the details section of \link{eval.calc_perf}.
#' 
#' 
#' @examples
#' \dontrun{
#' # ranking for all Bayesian WSP tests 
#' # input perf_b loaded or generated with eval.calc_perf_b
#' ranking_b = eval.rank_auc_b(perf_b)
#' # effects of DGP parameters on AUC of top test
#' ranking_b
#' 
#' # ranking for all frequentist WSP tests
#' # perf_f loaded or generated with eval.calc_perf_f
#' ranking_f = eval.rank_auc_f(perf_f)
#' # effects of DGP parameters on AUC of top test
#' ranking_f
#' 
#' # merging AUC ranking of Bayesian and frequentist tests
#' ranking = dplyr::bind_rows(ranking_b$ranking[, 1:8], 
#'                            ranking_f$ranking[, 1:5]) %>%
#'           filter(tte.dist == "pgw") %>%
#'           dplyr::arrange(dplyr::desc(AUC))
#'       
#'     
#' # ranking among subset of scenarios, e.g. excluding model choices that lead to
#' # long execution times
#' auc_pgw_ll <- dplyr::filter(perf_b, tte.dist == "pgw", prior.dist == "ll")
#' eval.rank_auc_b(auc_dw_ll) 
#'                                
#' }
#' 
#'
#' @name eval.rank_auc
#'
NULL


#' @rdname eval.rank_auc
#' 
#' @export
#'


# for the Bayesian tests 

eval.rank_auc_b = function(perf_b){
  require(dplyr)
 
  # 0. argument checks
  # TODO
  
  tab.df = perf_b
  
  # 1. summarize and rank fit & test specifications
  
  # ranking under correct prior belief
  tab.ranked <- tab.df %>%
    dplyr::filter(dist.prior.to.truth == "correct specification") %>% # only for correct prior spec for now
    dplyr::group_by(tte.dist, prior.dist, post.ci.type, cred.level, sensitivity.option) %>% # group by fit&test specifications
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(AUC)) # ranking by AUC

  
  # 2. filter for best fit & test specification and investigate effect of scenario parameters 
  
  opti.tte.dist = tab.ranked$tte.dist[1]
  opti.prior.dist = tab.ranked$prior.dist[1]
  opti.post.ci.type = tab.ranked$post.ci.type[1]
  opti.cred.level = tab.ranked$cred.level[1]
  opti.sensitivity.option = tab.ranked$sensitivity.option[1]
  
  
  # optimal fit & test specification according to ranking under correct prior belief
  tab.opti = tab.df %>% filter(tte.dist == opti.tte.dist, 
                               prior.dist == opti.prior.dist,
                               post.ci.type == opti.post.ci.type,
                               cred.level == opti.cred.level,
                               sensitivity.option == opti.sensitivity.option,
                               dist.prior.to.truth == "correct specification")
  
  # Effect of N
  tab.opti.N = tab.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(N) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop")
  
  # Effect of br
  tab.opti.br = tab.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(br) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop")
  
  # Effect of adr.rate
  tab.opti.adr.rate = tab.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(adr.rate) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop")
  
  # Effect of adr.when
  tab.opti.adr.when = tab.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(adr.when) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop")
  
  # Effect of adr.relsd
  tab.opti.adr.relsd = tab.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(adr.relsd) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop")
  
  # 3. Robustness wrt prior specification
  
  # Effect of distance of prior belief to true adr.when
  tab.robu.dist.prior.to.truth = tab.df %>% 
    filter(tte.dist == opti.tte.dist, 
           prior.dist == opti.prior.dist,
           post.ci.type == opti.post.ci.type,
           cred.level == opti.cred.level,
           sensitivity.option == opti.sensitivity.option) %>% 
    group_by(dist.prior.to.truth) %>% 
    dplyr::summarise(AUC = mean(auc), FPR = mean(fpr), TPR = mean(tpr), FNR = mean(fnr), TNR = mean(tnr), 
                     .groups = "drop")
  
  out = list(ranking = tab.ranked,
             effect.of.N = tab.opti.N,
             effect.of.br = tab.opti.br,
             effect.of.adr.rate = tab.opti.adr.rate,
             effect.of.adr.when = tab.opti.adr.when,
             effect.of.adr.relsd = tab.opti.adr.relsd,
             effect.of.dist.prior.to.truth = tab.robu.dist.prior.to.truth
  )
  return(out)
  
}




#' @rdname eval.rank_auc
#' 
#' @export
#'


## fct for frequentist tests

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
