#'
#'
#'@export

monitor_progress = function(pc_table,
                            wd = "D:/J/Sciebo/bADR_simstudyres01",
                            batch_max = 10){
  # prep
  run_count = matrix(rep(0, nrow(pc_table)*batch_max), ncol = batch_max)
  rownames(run_count) = 1:nrow(pc_table)
  for(i in 1:nrow(run_count)){
    for(j in 1:batch_max){
      out <- tryCatch(
        {
          # 'tryCatch()' will return the last evaluated expression
          # in case the "try" part was completed successfully
          message("File exists.")
          load_scenario(pc = pc_table[i,], wd= wd, batchnr = j)
          1
          # You don't need to state the return value via `return()` as code
          # in the "try" part is not wrapped inside a function (unlike that
          # for the condition handlers for warnings and error below)
        },
        error=function(cond) {
          message("File does not exist")
          # Choose a return value in case of error
          return(0)
        }
      )
      run_count[i,j] = out
    }
  }
  progress = rowSums(run_count)
  return(progress)
}


#progress8 = monitor_progress(pc_table = pc.numeric)
#View(progress8)
#save(progress8, file = "progress_2023_11_20.RData")
#library(dplyr)
#library(tidyverse)
#which(progress8 == 0) %>% pc[.,]
#which(progress8 < 10 & progress8 > 0) %>% pc[.,]
