#' Calculate Performance Metrics for FWSP Tests
#'
#' Computes the area under the ROC curve (AUC) as well as false positive, true positive, 
#' false negative and true negative rates for all FWSP test configurations defined by
#' tte distribution (\code{tte.dist}) and `pc_list$input$cred.level`, 
#' across simulated scenarios.
#'
#' @param pc_list A list with simulation parameters, test configurations, and file paths 
#'   (see \code{\link{sim.setup_simpars}}).
#'
#' @return A data frame with one row per ADR-positive scenario and FWSP test configuration, 
#'   including AUC and related performance metrics.
#'
#' @details Metrics are calculated using the `ROCR` package for each scenario with equal 
#'   numbers of ADR-positive and control simulations. Scenarios with insufficient 
#'   repetitions return `NA`.
#'
#' @seealso \code{\link{fwsp_test}}, \code{\link{eval.rank_auc}}
#'
#' @references
#' \insertAllCited{}
#'
#' @export


eval.calc_perf_f = function(pc_list) {
  require(dplyr)
  
  # Load results
  if (!exists("res_f")) {
    tryCatch({
      load(paste0(pc_list$add$resultpath, "/res_f.RData"))
      message("res_f.RData successfully loaded")
    }, error = function(cond) {
      sim.merge_results(pc_list, save = TRUE, bayes = FALSE)
      load(paste0(pc_list$add$resultpath, "/res_f.RData"))
      message("Batches merged and loaded")
    })
  } else {
    message("Object `res_f` currently loaded in environment is used to calculate performance measures.")
  }
  
  res_f$lab = ifelse(res_f$adr.rate > 0, 1, 0)
  res.ext = res_f
  
  pc.pos = filter(pc_list$pc_table, adr.rate > 0)
  pc.pos = unique(pc.pos[, -c(7:9)])  # Drop prior info (frequentist)
  
  fwsp_cols = grep("^fwsp_", names(res.ext), value = TRUE)
  nr.combined.tests = length(fwsp_cols)
  
  aucs = fprs = tprs = fnrs = tnrs = 
    matrix(NA, nrow = nrow(pc.pos), ncol = nr.combined.tests)
  colnames(aucs) = sub("^fwsp", "auc", fwsp_cols)
  colnames(fprs) = sub("^fwsp", "fpr", fwsp_cols)
  colnames(tprs) = sub("^fwsp", "tpr", fwsp_cols)
  colnames(fnrs) = sub("^fwsp", "fnr", fwsp_cols)
  colnames(tnrs) = sub("^fwsp", "tnr", fwsp_cols)
  
  run.reps = c()
  
  for (i in 1:nrow(pc.pos)) {
    scenario <- pc.pos[i, ]
    
    res.test = res.ext %>%
      filter((adr.rate == 0 | adr.rate == scenario$adr.rate),
             (is.na(adr.when) | adr.when == scenario$adr.when),
             N == scenario$N,
             br == scenario$br,
             (is.na(adr.relsd) | adr.relsd == scenario$adr.relsd))
    
    run.reps[i] = nrow(res.test)
    
    labels = matrix(res.test$lab, nrow = run.reps[i], ncol = nr.combined.tests, byrow = FALSE)
    predictions = as.matrix(res.test[, fwsp_cols])
    predictions[is.na(predictions)] = 0
    
    # Calculate AUCs
    pred.obj = ROCR::prediction(predictions, labels)
    aucs[i, ] = ROCR::performance(pred.obj, "auc")@y.values %>% unlist()
    
    # Calculate tp, fp, tn, fn
    
    tp = colSums(predictions == 1 & labels == 1)
    fp = colSums(predictions == 1 & labels == 0)
    tn = colSums(predictions == 0 & labels == 0)
    fn = colSums(predictions == 0 & labels == 1)
    
    tprs[i, ] = tp / (tp + fn)
    fprs[i, ] = fp / (fp + tn)
    fnrs[i, ] = fn / (tp + fn)
    tnrs[i, ] = tn / (fp + tn)
  }
  
  fprs = cbind(pc.pos, fprs)
  tprs = cbind(pc.pos, tprs)
  fnrs = cbind(pc.pos, fnrs)
  tnrs = cbind(pc.pos, tnrs)
  aucs = cbind(pc.pos, aucs)
  
  # reshape performance measure matrices to long format
  
  fpr_cols <- grep("^fpr_", names(fprs), value = TRUE)
  # Reshape
  fprs_long <- reshape(
    fprs[, c(setdiff(names(fprs), fpr_cols), fpr_cols)],
    varying = fpr_cols,
    v.names = "fpr",
    timevar = "test_spec",
    times = fpr_cols,
    direction = "long"
  )
  rownames(fprs_long) <- NULL
  # Extract tte.dist, and cred.level from test_spec
  fprs_long$tte.dist <- sub(".*_(w|dw|pgw).*", "\\1", fprs_long$test_spec)
  fprs_long$cred.level <- as.numeric(sub(".*_(\\d+\\.?\\d*)$", "\\1", fprs_long$test_spec))
  
  tpr_cols <- grep("^tpr_", names(tprs), value = TRUE)
  # Reshape
  tprs_long <- reshape(
    tprs[, c(setdiff(names(tprs), tpr_cols), tpr_cols)],
    varying = tpr_cols,
    v.names = "tpr",
    timevar = "test_spec",
    times = tpr_cols,
    direction = "long"
  )
  rownames(tprs_long) <- NULL
  # Extract tte.dist, and cred.level from test_spec
  tprs_long$tte.dist <- sub(".*_(w|dw|pgw).*", "\\1", tprs_long$test_spec)
  tprs_long$cred.level <- as.numeric(sub(".*_(\\d+\\.?\\d*)$", "\\1", tprs_long$test_spec))
  
  fnr_cols <- grep("^fnr_", names(fnrs), value = TRUE)
  # Reshape
  fnrs_long <- reshape(
    fnrs[, c(setdiff(names(fnrs), fnr_cols), fnr_cols)],
    varying = fnr_cols,
    v.names = "fnr",
    timevar = "test_spec",
    times = fnr_cols,
    direction = "long"
  )
  rownames(fnrs_long) <- NULL
  # Extract tte.dist, and cred.level from test_spec
  fnrs_long$tte.dist <- sub(".*_(w|dw|pgw).*", "\\1", fnrs_long$test_spec)
  fnrs_long$cred.level <- as.numeric(sub(".*_(\\d+\\.?\\d*)$", "\\1", fnrs_long$test_spec))
  
  tnr_cols <- grep("^tnr_", names(tnrs), value = TRUE)
  # Reshape
  tnrs_long <- reshape(
    tnrs[, c(setdiff(names(tnrs), tnr_cols), tnr_cols)],
    varying = tnr_cols,
    v.names = "tnr",
    timevar = "test_spec",
    times = tnr_cols,
    direction = "long"
  )
  rownames(tnrs_long) <- NULL
  # Extract tte.dist, and cred.level from test_spec
  tnrs_long$tte.dist <- sub(".*_(w|dw|pgw).*", "\\1", tnrs_long$test_spec)
  tnrs_long$cred.level <- as.numeric(sub(".*_(\\d+\\.?\\d*)$", "\\1", tnrs_long$test_spec))
  
  # reshape aucs to long format
  auc_cols <- grep("^auc_", names(aucs), value = TRUE)
  # Reshape
  aucs_long <- reshape(
    aucs[, c(setdiff(names(aucs), auc_cols), auc_cols)],
    varying = auc_cols,
    v.names = "auc",
    timevar = "test_spec",
    times = auc_cols,
    direction = "long"
  )
  rownames(aucs_long) <- NULL
  # Extract tte.dist, and cred.level from test_spec
  aucs_long$tte.dist <- sub(".*_(w|dw|pgw).*", "\\1", aucs_long$test_spec)
  aucs_long$cred.level <- as.numeric(sub(".*_(\\d+\\.?\\d*)$", "\\1", aucs_long$test_spec))
  
  # merge performance measure matrices fprs, tprs, auc to one
  # merge fprs and tprs
  pm_long = dplyr::left_join(fprs_long, tprs_long, by = c(
    "N", "br", "adr.rate", "adr.when", "adr.relsd", "study.period", 
    "tte.dist", "cred.level"
  ))
  # merge fprs, tprs and fnrs
  pm_long = dplyr::left_join(pm_long, fnrs_long, by = c(
    "N", "br", "adr.rate", "adr.when", "adr.relsd", "study.period", 
    "tte.dist", "cred.level"
  ))
  # merge fprs, tprs, fnrs and tnrs
  pm_long = dplyr::left_join(pm_long, tnrs_long, by = c(
    "N", "br", "adr.rate", "adr.when", "adr.relsd", "study.period", 
    "tte.dist", "cred.level"
  ))
  # merge fprs, tprs, fnrs, tnrs and aucs
  # (this is the final result)
  pm_long = dplyr::left_join(pm_long, aucs_long, by = c(
    "N", "br", "adr.rate", "adr.when", "adr.relsd", "study.period", 
    "tte.dist", "cred.level"
  ))
  
  # select relevant columns:
  pm_long <- pm_long[, c(
    "N", "br", "adr.rate", "adr.when", "adr.relsd", "study.period", 
    "tte.dist", "cred.level", 
    "auc", "fpr", "tpr", "fnr", "tnr"
  )]
  
  return(pm_long)
  
}

## END OF DOC