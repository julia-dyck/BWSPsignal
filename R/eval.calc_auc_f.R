#' Calculate AUC of FWSP Test specifications 
#'
#' Calculates the area under curve (AUC) of the receiver operating characteristic
#' (ROC) graph with one threshold \insertCite{fawcett2004,lloyd1998}{BWSPsignal} 
#' for all combinations of BWSP test configurations defined in `pc_list$test` and
#' all simulated scenarios. 
#'
#' @param pc_list A list containing simulation parameters, test settings, and output paths 
#'   (see \code{\link{sim.setup_simpars}} for structure).
#'
#' @return A data frame with one row per ADR-positive simulation scenario, BWSP 
#' test configuration and corresponding AUC value.
#'
#' @details The function calculates AUC using the `ROCR` package. 
#'   The AUCs are computed for each simulation scenario based on equal numbers of 
#'   ADR and no-ADR scenarios.
#'   Scenarios where the number of repetitions deviates from the targeted count 
#'   (e.g. due to convergence issues) are returned with `NA` AUCs.
#'   
#'   The output table provides the base for a ranking of model and test configurations 
#'   based on the auc ( see \code{\link{eval.rank_auc}}).
#'
#' @seealso \code{\link{fwsp_test}}
#' @seealso \code{\link{eval.rank_auc}}
#' 
#' @references 
#' \insertAllCited{}
#' 
#'
#' @export

eval.calc_auc_f = function(pc_list){
  require(dplyr) # for the pipe operator
  
  # 0. -------------------------------------------------------------------------
  #### load res table
  if (!exists("res_f")) { 
    # obtain res table
    tryCatch({
      load(paste0(pc_list$add$resultpath, "/res_f.RData"))
      message("res_f.RData successfully loaded")
    }, error = function(cond) {
      sim.merge_results(pc_list, save = T, bayes = F)
      load(paste0(pc_list$add$resultpath, "/res_f.RData"))
      message(" batches merged and loaded")
    })
  }
  else{
    message("Object `res_f` loaded in current environment is used to calculate aucs.")
  }
  
  # 1. -------------------------------------------------------------------------
  #### add label for true adr status
  res_f$lab = ifelse(res_f$adr.rate > 0, 1, 0) # 1 = ADR, 0 = no ADR)
  res.ext = res_f
  
  # 2. -------------------------------------------------------------------------
  #### calculate AUC for each simulation scenario (= one row of pc_list$pc_table)
  
  ## control cases are matched to each ADR-positive scenario for AUC calc
  pc.pos = filter(pc_list$pc_table, adr.rate > 0) # only ADR-positive scenarios
  pc.pos = unique(pc.pos[,-(7:9)])  # remove prior.dist & prior.belief 
                                    # (irrelevant in frequentist approach)
  # number of tests
  nr.combined.tests = length(grep("^fwsp_", names(res.ext))) 
  
  # Identify all fwsp_test result columns
  fwsp_cols = grep("^fwsp_", names(res.ext), value = TRUE)
  
  # prep empty matrix for AUC results  
  aucs = matrix(rep(NA, nr.combined.tests*nrow(pc.pos)), ncol = nr.combined.tests)
  colnames(aucs) = sub("^fwsp", "auc", fwsp_cols)
  
  run.reps = c()
  
  # go through every ADR-positive scenario linked with control
  for(i in 1:nrow(pc.pos)){
    N_i = pc.pos$N[i]
    br_i = pc.pos$br[i]
    adr.rate_i = pc.pos$adr.rate[i]
    adr.when_i = pc.pos$adr.when[i]
    adr.relsd_i = pc.pos$adr.relsd[i]
    # no study.period, as supposed to be only one value
    # no tte.dist, as w, dw and pgw are indicated in colnames
    # no prior.dist, as irrelevant in frequentist modelling
    # no prior.belief, as irrelevant in frequentist modelling
    
    res.test = res.ext %>%
      dplyr::filter((adr.rate == 0 | adr.rate == adr.rate_i),
                    (is.na(adr.when) | adr.when == adr.when_i),
                    N == N_i,
                    br == br_i,
                    (is.na(adr.relsd) | adr.relsd == adr.relsd_i)
                    )
    
    run.reps[i] = nrow(res.test) # number of repetitions obtained for this scenario
    
    
    # set up labels and predictions in a matrix
    labels = matrix(res.test$lab, nrow = run.reps, ncol = nr.combined.tests, byrow = F)
    predictions = data.frame(res.test)[,fwsp_cols] %>% as.matrix()
    pred.with.na = predictions
    predictions[is.na(predictions)] = 0 # NA handling pgWSP test 
                                        # (nonconverged mod -> interpreted as no signal)
    
    # creating prediction object
    pred.obj <- ROCR::prediction(predictions, labels)
    
    # calculate AUCs
    aucs[i,] = ROCR::performance(pred.obj, "auc") %>%  ## HIER VLLT OPTION FÜR FP; TP EINBAUEN; UM ROC CURVE DARAUS ZU PLOTTEN (ARGUMENT DAFÜR EINRICHTEN)
      .@y.values %>%
      as.numeric()
  }

  # 5. -------------------------------------------------------------------------
  #### add info and reshape table forma
  
  
  aucs = cbind(pc.pos, run.reps, aucs)
  return(aucs)
  
  # TODO: after long format: add new tte.dist col (extract with grep)
  # MAYBE: add col with no. of tests going into the auc calculation? -> talk to Odile
  
  
  # reshape to long format
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
  
  # Extract post.ci.type, cred.level, and sensitivity.option from test_spec
  aucs_long$post.ci.type = sub("^auc_([^_]+)_.*", "\\1", aucs_long$test_spec)
  aucs_long$cred.level <- as.numeric(sub("^auc_[^_]+_([0-9\\.]+)_.*", "\\1", aucs_long$test_spec))
  aucs_long$sensitivity.option <- as.integer(sub(".*opt([0-9]+)$", "\\1", aucs_long$test_spec))
  
  # select relevant columns:
  aucs_long <- aucs_long[, c(
    "N", "br", "adr.rate", "adr.when", "adr.relsd", "study.period", 
    "tte.dist", "prior.dist", "prior.belief", "dist.prior.to.truth",
    "post.ci.type", "cred.level", "sensitivity.option", "auc"
  )]
  
  return(aucs_long)
  
}

## END OF DOC