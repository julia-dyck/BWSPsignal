#' Evaluation: rank FWSP tests by AUC
#'
#' Ranks all tried fit & test specifications by AUC.
#'
#' @param auc_f A data frame containing Bayesian AUC results returned by \link{eval.calc_auc_f}.
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


eval.rank_auc_f = function(auc_f){
  require(dplyr)
  
  auc.df = auc_f
  
  # 1. summarize and rank fit & test specifications
  
  # ranking
  auc.ranked <- auc.df %>%
    dplyr::group_by(tte.dist, cred.level) %>% # group by fit&test specifications
    dplyr::summarise(AUC = mean(auc), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(AUC)) # ranking
  
 
  # 2. filter for best fit & test specification and investigate effect of scenario parameters 
  
  opti.tte.dist = auc.ranked$tte.dist[1]
  opti.cred.level = auc.ranked$cred.level[1]
   
  # optimal fit & test specification according to ranking under correct prior belief
  auc.opti = auc.df %>% filter(tte.dist == opti.tte.dist, 
                               cred.level == opti.cred.level,
                               )
  
  # Effect of N
  auc.opti.N = auc.opti %>% 
    group_by(N) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of br
  auc.opti.br = auc.opti %>% 
    group_by(br) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of adr.rate
  auc.opti.adr.rate = auc.opti %>% 
   group_by(adr.rate) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of adr.when
  auc.opti.adr.when = auc.opti %>% 
    group_by(adr.when) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of adr.relsd
  auc.opti.adr.relsd = auc.opti %>% 
    group_by(adr.relsd) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Output 
  out = list(
    ranking = auc.ranked,
    effect.of.N = auc.opti.N,
    effect.of.br = auc.opti.br,
    effect.of.adr.rate = auc.opti.adr.rate,
    effect.of.adr.when = auc.opti.adr.when,
    effect.of.adr.relsd = auc.opti.adr.relsd
  )
  return(out)
  
}

