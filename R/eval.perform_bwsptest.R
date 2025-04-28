
eval.calc_auc = function(pc_list){
  # 1. -------------------------------------------------------------------------
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
  
  return(res.ext)  ## TODO: CHECK WHETHER THE TESTS REALLY ARE CONDUCTED CORRECTLY 
  
  
}

tests = eval.calc_auc(pc_list)
View(tests)
