
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
  
  
  
  
  return(res.ext)           
  
  
  
  
  
}


res.ext = eval.calc_auc(pc_list)
