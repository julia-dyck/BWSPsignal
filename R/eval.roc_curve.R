#' Plot ROC Curves for Top Test Specifications
#'
#' Plots ROC curves with shaded areas under the curve for the top-ranked test specifications 
#' (based on AUC).
#'
#' @param rank_tab A data frame of ranked test specifications obtained from 
#' \link{`eval.rank_auc_b()`} or \link{`eval.rank_auc_f()`}.
#' @param n The number of top-ranked test specifications to plot (default = 10).
#'
#' @return A ggplot object displaying ROC curves with shaded AUC regions.
#' 
#' @details add details and references for ROC curves...
#' 
#'
#' @examples
#' \dontrun{
#' ranking_b # output of eval.rank_auc_b()
#' eval.roc_curve(ranking_b$ranking, n = 3)
#' 
#' ranking_f # output of eval.rank_auc_f()
#' eval.roc_curve(ranking_b$ranking, n = 3)
#' 
#' # for a comparative study of Bayesian and Frequentist tests 
#' ranking = dplyr::bind_rows(ranking_b$ranking[, 1:8], 
#'                            ranking_f$ranking[, 1:5]) %>%
#'   filter(tte.dist == "pgw") %>%
#'   dplyr::arrange(dplyr::desc(AUC))
#' 
#' }
#'
#' @export

eval.roc_curve = function(rank_tab, n = 10) {
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  n = min(n, nrow(rank_tab))  # safety
  
  print(rank_tab[1:n,])
  
  # Add combined label
  rank_tab = rank_tab %>%
    dplyr::slice(1:n) %>%
    dplyr::mutate(
      spec = paste(tte.dist, prior.dist, post.ci.type, cred.level, sensitivity.option, sep = " | "),
      spec = factor(spec, levels = unique(spec))  # preserve legend order
    )
  
  # Build long-format ROC + polygon bottom closure
  roc_df = rank_tab %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      FPR_full = list(c(0, FPR, 1, 1, 0)),  # extended polygon
      TPR_full = list(c(0, TPR, 1, 0, 0))
    ) %>%
    tidyr::unnest(cols = c(FPR_full, TPR_full)) %>%
    dplyr::group_by(spec) %>%
    dplyr::mutate(point = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  # Base ROC curve points for line/points (without bottom closure)
  line_df = rank_tab %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      FPR_vec = list(c(0, FPR, 1)),
      TPR_vec = list(c(0, TPR, 1))
    ) %>%
    tidyr::unnest(cols = c(FPR_vec, TPR_vec)) %>%
    dplyr::mutate(spec = factor(paste(tte.dist, prior.dist, post.ci.type, cred.level, sensitivity.option, sep = " | "),
                                levels = unique(rank_tab$spec))) %>%
    dplyr::group_by(spec) %>%
    dplyr::mutate(point = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  ggplot2::ggplot() +
    # ROC area (polygon to bottom)
    ggplot2::geom_polygon(data = roc_df,
                          ggplot2::aes(x = FPR_full, y = TPR_full, group = spec, fill = spec),
                          alpha = 0.2, color = NA) +
    
    # ROC line
    ggplot2::geom_line(data = line_df,
                       ggplot2::aes(x = FPR_vec, y = TPR_vec, color = spec, group = spec),
                       size = 1.2) +
    
    # ROC points
    ggplot2::geom_point(data = line_df,
                        ggplot2::aes(x = FPR_vec, y = TPR_vec, color = spec, group = spec),
                        size = 2) +
    
    # Diagonal line
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
    
    ggplot2::labs(
      x = "False Positive Rate (FPR)",
      y = "True Positive Rate (TPR)",
      color = "Test specification \n(descending ranking order)",
      fill = "Test specification \n(descending ranking order)",
      title = paste0("ROC curves of top ", n, " test specifications")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right")
  
}


## END OF DOC