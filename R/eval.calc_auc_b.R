#' Calculate AUC of BWSP Test specifications 
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
#' @details The function performs BWSP tests across all specified posterior CI types, credibility levels, 
#'   and sensitivity options for each simulation run, and calculates AUC using the `ROCR` package. 
#'   The AUCs are computed for each simulation scenario based on equal numbers of 
#'   ADR and no-ADR scenarios.
#'   Scenarios where the number of repetitions deviates from the targeted count 
#'   (e.g. due to convergence issues) are returned with `NA` AUCs.
#'   
#'   The output table provides the base for a ranking of model and test configurations 
#'   based on the auc ( see \code{\link{eval.rank_auc}}).
#'
#' @seealso \code{\link{bwsp_test}}
#' @seealso \code{\link{eval.rank_auc}}
#' 
#' @references 
#' \insertAllCited{}
#' 
#'
#' @export


eval.calc_auc_b = function(pc_list)
                           {
  require(dplyr) # for the pipe operator
  
  # 0. -------------------------------------------------------------------------
  #### load res table
  if (!exists("res_b")) { 
    # obtain res table
    tryCatch({
      load(paste0(pc_list$add$resultpath, "/res_b.RData"))
      message("res_b.RData successfully loaded")
    }, error = function(cond) {
      sim.merge_results(pc_list, save = T)
      load(paste0(pc_list$add$resultpath, "/res_b.RData"))
      message(" batches merged and loaded")
    })
  }
  else{
    message("Object `res_b` loaded in current environment is used to calculate aucs.")
  }
  
  # 1. -------------------------------------------------------------------------
  #### add label for true adr status
  res_b$lab = ifelse(res_b$adr.rate > 0, 1, 0) # 1 = ADR, 0 = no ADR)
  
  # 2. -------------------------------------------------------------------------
  #### add ropes for all bwsp tests to be performed
  
  rope.infos = dplyr::filter(do.call(dplyr::bind_rows, pc_list$fit), prior.belief == "none")
  
  # calculate ropes for each tte.dist & prior.dist combi
  ropes = do.call(rbind, lapply(1:nrow(rope.infos), function(i) {
    # extract row
    rope.infos.row = rope.infos[i, ]
    # apply eval.calc_rope
    ropes = eval.calc_rope(cred.levels = pc_list$input$cred.level, rope.infos.row = rope.infos.row)
    return(ropes)
  }))
  
  rope.infos$prior.belief <- NULL # to not mess up the merge
  # remove cols unnecessary? (at the same time, they do not hurt)
  rope.infos = cbind(rope.infos, ropes)
  
  res.ext = merge(res_b, rope.infos, by = c("tte.dist", "prior.dist"), all.x = TRUE) #merge
  
  # 3. -------------------------------------------------------------------------
  #### perform all bwsp tests and save binary test result per row and per test specification as new res cols
  
  # names of the lower and upper interval bounds you need
  cred.levels = pc_list$input$cred.level
   
  # posterior CI types
  types = tolower(pc_list$input$post.ci.type)
  
  # define the different sensitivity options
  options = pc_list$input$sensitivity.option
  
  
  # loop over sensitivity options
  for (opt in options) {
    
    # loop over types (hdi, eti)
    for (type in types) {
      
      # loop over credibility levels
      for (lev in cred.levels) {
        
        # create the column names dynamically for credregion
        cred_cols = c(
          paste0("nu.", type, lev, "l"),
          paste0("nu.", type, lev, "u"),
          paste0("ga.", type, lev, "l"),
          paste0("ga.", type, lev, "u")
        )
        
        # create the column names dynamically for nullregion
        null_cols = c(
          paste0("nu.", "rope", lev, "l"),
          paste0("nu.", "rope", lev, "u"),
          paste0("ga.", "rope", lev, "l"),
          paste0("ga.", "rope", lev, "u")
        )
        
        # define a new column name for the result
        bwsp_col = paste0("bwsp_", type, "_", lev, "_opt", opt)
        
        # apply the test rowwise
        res.ext[[bwsp_col]] = apply(
          res.ext[, c(cred_cols, null_cols, "tte.dist")], 
          MARGIN = 1,
          FUN = function(x) {
            # first 4 entries = credregion, next 4 entries = nullregion
            credregion = as.numeric(unlist(x[1:4]))
            nullregion = as.numeric(unlist(x[5:8]))
            tte.dist = x[9]
            
            if(tte.dist == "dw" || tte.dist == "pgw"){
              bwsp_test(
                credregion = credregion,
                nullregion = nullregion,
                option = opt,
                mod =  tte.dist
              )
            } else if(tte.dist == "w"){
              bwsp_test(
                credregion = credregion[1:2],
                nullregion = nullregion[1:2],
                option = opt,
                mod =  tte.dist
              )
            }
          }
        ) # end of apply
      } # end of loop over cred.levels
    } # end of loop over cred.levels
  } # end of loop over options

  
  # 4. -------------------------------------------------------------------------
  #### calculate AUC for each simulation scenario (= one row of pc_list$pc_table)
  
  ## control cases are matched to each ADR-positive scenario for AUC calc
  pc.pos = filter(pc_list$pc_table, adr.rate > 0) # only ADR-positive scenarios
  
  # number of tests
  nr.combined.tests = length(grep("^bwsp_", names(res.ext))) 

  # Identify all bwsp_test result columns
  bwsp_cols = grep("^bwsp_", names(res.ext), value = TRUE)
  
  # prep empty matrix for performance measure results
  # false positive rate
  fprs = matrix(rep(NA, nr.combined.tests*nrow(pc.pos)), ncol = nr.combined.tests)
  colnames(fprs) = sub("^bwsp", "fpr", bwsp_cols)
  # true positive rate
  tprs = matrix(rep(NA, nr.combined.tests*nrow(pc.pos)), ncol = nr.combined.tests)
  colnames(tprs) = sub("^bwsp", "tpr", bwsp_cols)
  # area under the receiver operating characteristic (ROC) curve
  aucs = matrix(rep(NA, nr.combined.tests*nrow(pc.pos)), ncol = nr.combined.tests)
  colnames(aucs) = sub("^bwsp", "auc", bwsp_cols)
  
  run.reps = c()
  # go through every ADR-positive scenario linked with control
  for(i in 1:nrow(pc.pos)){
    N_i = pc.pos$N[i]
    br_i = pc.pos$br[i]
    adr.rate_i = pc.pos$adr.rate[i]
    adr.when_i = pc.pos$adr.when[i]
    adr.relsd_i = pc.pos$adr.relsd[i]
    # no study.period, as supposed to be only one value
    tte.dist_i = pc.pos$tte.dist[i]
    prior.dist_i = pc.pos$prior.dist[i]
    prior.belief_i = pc.pos$prior.belief[i]
    
    res.test = res.ext %>%
      dplyr::filter((adr.rate == 0 | adr.rate == adr.rate_i),
             (is.na(adr.when) | adr.when == adr.when_i),
             N == N_i,
             br == br_i,
             (is.na(adr.relsd) | adr.relsd == adr.relsd_i),
             tte.dist == tte.dist_i,
             prior.dist == prior.dist_i,
             prior.belief == prior.belief_i)
    
    run.reps[i] = nrow(res.test) # number of repetitions obtained for this scenario
    if(run.reps[i] == 2*pc_list$add$reps){
      
      # set up labels and predictions in a matrix
      labels = matrix(res.test$lab, nrow = run.reps[i], ncol = nr.combined.tests, byrow = F)
      predictions = data.frame(res.test)[,bwsp_cols] %>%
        as.matrix()
      
      # calculate tpr, fpr, tnr, fnr manually   ### HIER WEITER
      extract_performance = function(predictions, labels) {
        # true positives
        tp = rowSums(predictions == 1 & labels == 1)
        # false positives
        fp = rowSums(predictions == 1 & labels == 0)
        # true negatives
        tn = rowSums(predictions == 0 & labels == 0)
        # false negatives
        fn = rowSums(predictions == 0 & labels == 1)
        
        tpr = tp / (tp + fn) # true positive rate
        fpr = fp / (fp + tn) # false positive rate
        
        return(c(tpr, fpr))
      }
      
      
      # creating prediction object
      pred.obj <- ROCR::prediction(predictions, labels)

      
      # # calculate fprs with ROCR
      # fprs[i,] = ROCR::performance(pred.obj, "fpr") #%>%
      # return(fprs[i,])
      #   .@y.values %>%
      #   as.numeric()

      # calculate tprs with ROCR
      # tprs[i,] = ROCR::performance(pred.obj, "tpr") %>%
      #   .@y.values #%>%
      #   as.numeric()
      return(tprs = ROCR::performance(pred.obj, "tpr", "fpr"))
      
      # calculate AUCs
      aucs[i,] = ROCR::performance(pred.obj, "auc") %>%
        .@y.values %>%
        as.numeric()
    }
    else{
      #fprs[i,] = rep(NA, nr.combined.tests)
      tprs[i,] = rep(NA, nr.combined.tests)
      aucs[i,] = rep(NA, nr.combined.tests)
    }
  }
  return(list(fprs, tprs, aucs))
  # 5. -------------------------------------------------------------------------
  #### add info and reshape table format
  
  # add scenario information to aucs
  
  ## inner fct
  # add description of deviance between prior belief and simulated truth
  prior.correctness = function(pc_row) {
    adr.when = as.numeric(as.character(pc_row[4]))
    prior.belief = as.character(pc_row[9])
    out = ifelse((adr.when == 0.25 && prior.belief == "beginning") ||
                   (adr.when == 0.5 && prior.belief == "middle") ||
                   (adr.when == 0.75 && prior.belief == "end"), 
                 "correct specification", 
                 ifelse((adr.when == 0.25 && prior.belief == "middle") ||
                          (adr.when == 0.5 && prior.belief == "beginning") ||
                          (adr.when == 0.5 && prior.belief == "end") ||
                          (adr.when == 0.75 && prior.belief == "middle"),
                        "one quarter off",
                        ifelse((adr.when == 0.25 && prior.belief == "end") ||
                                 (adr.when == 0.75 && prior.belief == "beginning"),
                               "two quarters off",
                               "no ADR assumed")))
    return(out)
  }
  
  # add deviance to pc.pos (for grouped mean calculation later)
  dist.prior.to.truth = apply(pc.pos, 1, prior.correctness) 
  pc.pos = cbind(pc.pos, dist.prior.to.truth)
  
  fprs = cbind(pc.pos, fprs)
  tprs = cbind(pc.pos, tprs)
  aucs = cbind(pc.pos, aucs)
  return(list(fprs, tprs, aucs)) # HIER WEITER

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