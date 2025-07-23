#' Evaluation: rank FWSP tests by AUC
#'
#' Ranks all tried fit & test specifications by AUC.
#' 
#' # @param rank_tab_b A data frame containing the AUC values for Bayesian test specifications.
#' #' @param rank_tab_f A data frame containing the AUC values for Frequentist test specifications.
#' 
#' #' @return A data frame with the combined rankings of both Bayesian and Frequentist test specifications.
#' 
#' @export



eval.merge_rankings = function(rank_tab_b, rank_tab_f){
  
  # Combine the two rankings
  combined_ranking <- dplyr::bind_rows(rank_tab_b, rank_tab_f)
  
  # Arrange by descending AUC
  combined_ranking = dplyr::arrange(, combined_ranking, desc(AUC))
  
  return(combined_ranking)
}