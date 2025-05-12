#' Evaluation: rank BWSP tests by AUC
#'
#' Ranks all tried fit & test specifications by AUC.
#'
#' @param auc.df A data frame containing AUC results returned by \link{eval.calc_auc}.
#'
#' @return A list containing 
#' \itemize{
#' \item \code{$ranking}: Ranking of fit and BWSP test specifications according 
#' to AUC averaged over all sample scenarios given a correct specification of prior belief.
#' \item \code{$effect.of.N}: Effect of sample size on AUC for the optimal fit 
#' and BWSP test specification given a correct specification of prior belief.
#' \item \code{$effect.of.br}: Effect of background rate on AUC for the optimal fit
#' and BWSP test specification given a correct specification of prior belief.
#' \item \code{$effect.of.adr.rate}: Effect of ADR rate on AUC for the optimal fit
#' and BWSP test specification given a correct specification of prior belief.
#' \item \code{$effect.of.adr.when}: Effect of true expected event times on AUC 
#' for the optimal fit and BWSP test specification given a correct specification of prior belief.
#' \item \code{$effect.of.adr.relsd}: Effect of relative standard deviation of event 
#' time on AUC for the optimal fit and BWSP test specification given a correct 
#' specification of prior belief.
#' \item \code{$effect.of.dist.prior.to.truth}: Effect of distance of prior belief 
#' to true adr.when on AUC for the optimal fit and BWSP test specification given a 
#' correct specification of prior belief.
#' }
#' 
#' 
#' @examples
#' \dontrun{
#' # Filtering for a specific tte and prior distribution (for instance based on
#' # evaluation of execution times, convergence and 
#'  auc_dw_ll <- dplyr::filter(aucs, tte.dist == "dw", prior.dist == "ll")
#'  eval.rank_auc(auc_dw_ll)
#' }
#' @export


eval.rank_auc = function(auc.df){
  require(dplyr)
  
  # 1. group by fit & test specifications

  auc.ranked <- auc.df %>%
    dplyr::filter(dist.prior.to.truth == "correct specification") %>% # only for correct prior spec for now
    dplyr::group_by(tte.dist, prior.dist, post.ci.type, cred.level, sensitivity.option) %>% # group by fit&test specifications
    dplyr::summarise(AUC = mean(auc), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(AUC)) # ranking
  
  # 2. filter for best fit & test specification and investigate effect of scenario parameters 
  
  opti.tte.dist = auc.ranked$tte.dist[1]
  opti.prior.dist = auc.ranked$prior.dist[1]
  opti.post.ci.type = auc.ranked$post.ci.type[1]
  opti.cred.level = auc.ranked$cred.level[1]
  opti.sensitivity.option = auc.ranked$sensitivity.option[1]
  
  auc.opti = auc.df %>% filter(tte.dist == opti.tte.dist, 
                               post.ci.type == opti.post.ci.type,
                               cred.level == opti.cred.level,
                               sensitivity.option == opti.sensitivity.option,
                               dist.prior.to.truth == "correct specification")
  
  # Effect of N
  auc.opti.N = auc.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(N) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of br
  auc.opti.br = auc.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(br) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of adr.rate
  auc.opti.adr.rate = auc.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(adr.rate) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of adr.when
  auc.opti.adr.when = auc.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(adr.when) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # Effect of adr.relsd
  auc.opti.adr.relsd = auc.opti %>% 
    dplyr::filter(dist.prior.to.truth == "correct specification") %>%
    group_by(adr.relsd) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  # 3. Robustness wrt prior specification
  
  # Effect of distance of prior belief to true adr.when
  auc.opti.dist.prior.to.truth = auc.opti %>% 
    group_by(dist.prior.to.truth) %>% summarise(AUC = mean(auc), .groups = "drop")
  
  out = list(ranking = auc.ranked,
             effect.of.N = auc.opti.N,
             effect.of.br = auc.opti.br,
             effect.of.adr.rate = auc.opti.adr.rate,
             effect.of.adr.when = auc.opti.adr.when,
             effect.of.adr.relsd = auc.opti.adr.relsd,
             effect.of.dist.prior.to.truth = auc.opti.dist.prior.to.truth
             )
  return(out)
  
}

