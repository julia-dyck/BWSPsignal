#'
#'
#'
#' @export


eval.calc_auc = function(pc_list, 
                         dist.to.truth = sim.dist_to_truth_mat_default(pc_list))
                           {
  require(dplyr) # for the pipe operator
  
  # 0. -------------------------------------------------------------------------
  #### load res table
  if (!exists("res")) { 
    # obtain res table
    tryCatch({
      load(paste0(pc_list$add$resultpath, "/res.RData"))
      message("res.RData successfully loaded")
    }, error = function(cond) {
      sim.merge_results(pc_list, save = T)
      load(paste0(pc_list$add$resultpath, "/res.RData"))
      message(" batches merged and loaded")
    })
  }
  else{
    message("Object `res` loaded in current environment is used to extract effective sample sizes.")
  }
  
  # 1. -------------------------------------------------------------------------
  #### add label for true adr status
  res$lab = ifelse(res$adr.rate > 0, 1, 0) # 1 = ADR, 0 = no ADR)
  
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
  
  res.ext = merge(res, rope.infos, by = c("tte.dist", "prior.dist"), all.x = TRUE) #merge
  
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
  
  # add description of deviance between prior belief and simulated truth
  prior.correctness = function(pc_row) {
    adr.when = pc_row[4]
    prior.belief = pc_row[9]
    out = ifelse(adr.when == 0.25 && prior.belief == "beginning" ||
                 adr.when == 0.5 && prior.belief == "middle" ||
                 adr.when == 0.75 && prior.belief == "end", 
                               "correct specification", 
                               ifelse(adr.when == 0.25 && prior.belief == "middle" ||
                                      adr.when == 0.5 && prior.belief == "beginning" ||
                                      adr.when == 0.5 && prior.belief == "end" ||
                                      adr.when == 0.75 && prior.belief == "middle",
                                      "one quarter off",
                                      ifelse(adr.when == 0.25 && prior.belief == "end" ||
                                            adr.when == 0.75 && prior.belief == "beginning",
                                             "two quarters off",
                                              "no ADR assumed")))
    return(out)
  }
  # add deviance to pc.pos (for grouped mean calculation later)
  deviance.prior = apply(pc.pos, 1, prior.correctness) 
  pc.pos.ext = cbind(pc.pos, deviance.prior)
  
  # number of tests
  nr.combined.tests = length(grep("^bwsp_", names(res.ext))) 

 # grouping_vars = c("tte.dist", "prior.dist", "N", "br", "adr.rate", "adr.when", "adr.relsd", "study.period", "prior.belief")
  
  # Identify all bwsp_test result columns
  bwsp_cols = grep("^bwsp_", names(res.ext), value = TRUE)
  
  # prep empty matrix for AUC results  
  aucs = matrix(rep(NA, nr.combined.tests*nrow(pc.pos)), ncol = nr.combined.tests)
  colnames(aucs) = sub("^bwsp", "auc", bwsp_cols)
  
  
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
      filter(.$adr.rate %in% c(0, adr.rate_i),
             (is.na(.$adr.when) | .$adr.when == adr.when_i),
             .$N == N_i,
             .$br == br_i,
             (is.na(.$adr.relsd) | .$adr.relsd == adr.relsd_i),
             .$tte.dist == tte.dist_i,
             .$prior.dist == prior.dist_i,
             .$prior.belief == prior.belief_i)
    
    run.reps = nrow(res.test) # number of repetitions obtained for this scenario
    if(run.reps == 2*pc_list$add$reps){
      
      # set up labels and predictions in a matrix
      labels = matrix(res.test$lab, nrow = run.reps, ncol = nr.combined.tests, byrow = F)
      predictions = data.frame(res.test)[,bwsp_cols] %>%
        as.matrix()
      
      # creating prediction object
      pred.obj <- ROCR::prediction(predictions, labels)
      # calculate AUCs
      aucs[i,] = ROCR::performance(pred.obj, "auc") %>%
        .@y.values %>%
        as.numeric()
    }
    else{
      aucs[i,] = rep(NA, nr.combined.tests)
    }
  }
  
  
  
  
  
  return(aucs)
  
  
  
  # 5. -------------------------------------------------------------------------
  #### add distance to truth as grouping variable 

  # distance to truth depending on matrix in argument distance  to truth:
  
  
  
} # END OF FCT

# l = eval.calc_auc(pc_list)
# label_mat[,1:10]
# View(tests)
