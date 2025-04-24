
eval.calc_auc = function(pc_list){
  # load res
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
  
  
  # add rope lower and upper bound for to each row depending on 
  # ## tte.dist, 
  # ## prior.dist (+ prior mean + sd under prior.believe = "none"), 
  rope.infos = dplyr::filter(do.call(dplyr::bind_rows, pc_list$fit), prior.belief == "none")
  rope.infos = dplyr::mutate(rope.infos, 
                             rope.lower = NA, 
                             rope.upper = NA)
  return(rope.infos)
  
  
  # go through pc_list$test rows
  # extract ci_boundaries depending on post.ci.type & cred.level (using regex?)
  
  
}


# eval.calc_auc(pc_list)
