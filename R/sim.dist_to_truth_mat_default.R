#' Default specification of distance between adr.when.label and prior.belief
#' 
#' This function is used to create the default matrix for the specification of 
#' the distance between the simulated truth about the expected event time
#' represented by simulation parameter \code{adr.when.label} and \code{prior.belief}
#' about the expected event time.
#' 
#' @param pc_list A list containing the simulation parameters obtained from 
#' \link\code{sim.setup_sim_pars}.
#' 
#' @return A matrix with rows corresponding to \code{adr.when.label} and columns
#' corresponding to \code{prior.belief}. The values in the matrix are 
#' \enumerate{
#' \item 0 if simulated truth and prior.belief are matching,
#' \item 1 else.
#' }
#' 
#' 


sim.dist_to_truth_mat_default = function(pc_list){
  dist.mat = matrix(1, nrow = nrow(pc_list$add$adr.when.label), 
                    ncol = nrow(pc_list$add$adr.when.label))
  diag(dist.mat) = 0
  colnames(dist.mat) = pc_list$input$prior.belief
  rownames(dist.mat) = pc_list$input$prior.belief
  
  return(dist.mat)
}
