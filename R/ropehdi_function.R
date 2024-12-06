#' ROPE + HDR test
#'
#'
#'
#' @param nullregion a vector of length two denoting the lower and upper boundary of the null values defined region of practical equivalence (ROPE)
#' @param scale a vector with two entries denoting the lower and upper boundary of the posterior highest density region
#'
#' @return 0 if H_0 is accepted, 1 if H_0 is rejected and NA if no decision
#'
#' @details The decision rule is:
#'
#' - If the HDI falls completely inside the ROPE, then accept the null value for practical purposes.
#'
#' - If the HDI falls completely outside the ROPE, then reject the null value for practical purposes.
#'
#' - Else, withhold a decision.
#'
#'
#' For more information see for example
#'
#' J. K. Kruschke. Rejecting or accepting parameter values in Bayesian estimation. Advances in Methods and Practices in Psychological Science, 1(2):270–280, 2018.
#'
#' J. K. Kruschke. Doing Bayesian data analysis: a tutorial with R, JAGS, and Stan. Academic Press, 2015.
#'
#'
#' @export
#'




ropehdi = function(nullregion, credregion){
  # nullregion = region of practical equivalence representing the Null value,
  #              numeric vector of length 2, c(lowerboundary, upperboundary)
  # credregion = equal-tailed interval or highest posterior density region calculated
  #            from a posterior sample.
  #            matrix with two columns (1st for lower bounds, 2nd for upper bounds),
  #            and either one row if the region is an interval,
  #            or r rows, if the region is a union of r intervals
  nullreglow = nullregion[1]
  nullregupp = nullregion[2]
  credreglow = credregion[1]   # IMPLEMENT IT FOR SINGLE INTERVALS AT FIRST
  credregupp = credregion[2]

  # Cases of nullregion and credibility region: whether and how they intersect or not intersect

  if(nullreglow < credreglow){ # check: is the lower nullregion boundary left from lower credregion boundary?
    if(nullregupp < credreglow){ # meaning: Nullregion left from ci, no intersection
      rej.nullvalue = TRUE
      acc.nullvalue = FALSE
      # print("nullreg and credible interval disjunct, nullregion left")
    }
    else if(nullregupp > credreglow && nullregupp < credregupp){ # meaning: Nullregion left from cr, but partly intersecting
      rej.nullvalue = FALSE
      acc.nullvalue = FALSE
      # print("partial overlap, nullregion left")
    }
    else if(nullregupp > credregupp){ # meaning: Nullregion contains whole ci
      rej.nullvalue = FALSE
      acc.nullvalue = TRUE
      # print("nullreg contains the whole credible interval")
    }
  }
  else{ # check: else lower nullregion boundary is right from lower ci boundary
    if(nullreglow > credregupp){ # null region completely lies right from ci
      rej.nullvalue = TRUE
      acc.nullvalue = FALSE
      # print("nullreg and credible interval disjunct, nullregion right")
    }
    else if(nullregupp > credregupp){ # null region right from cr, but partly intersecting
      rej.nullvalue = FALSE
      acc.nullvalue = FALSE
      # print("partial overlap, null region right from credregion")
    }
    else if(nullregupp < credregupp){ # ci encompasses null region
      rej.nullvalue = FALSE
      acc.nullvalue = FALSE
      # print("nullregion encompassed by cred region")
    }
  }

  # rej.nullvalue when there is no intersection between null region and credibility region at all
  # acc.nullvalue when th credibility region lies completely within the null region
  # none TRUE, if credibility region and null region partly intersect
  # prop = proportion of the credibility interval that lies within the null region
  #        (0 refers to reject.nullvalue = T, 1 refers to acc.nullvalue = T,
  #        value between 0 and 1 refers to undecidedness)
  if(rej.nullvalue == FALSE && acc.nullvalue == FALSE){
    undecided = TRUE
  }
  else{
    undecided = FALSE
  }

  ### recode the output
  if(acc.nullvalue == TRUE){return(0)}
  if(rej.nullvalue == TRUE){return(1)}
  if(undecided == TRUE){return(NA)}

  #return(c(rej.nullvalue = rej.nullvalue, acc.nullvalue = acc.nullvalue, undecided = undecided))
}



# testing
# ropehdi(nullregion = c(1,2), credregion = c(3,5)) # null region left from hdi and disjunct
# 
# ropehdi(nullregion = c(1,3.5), credregion = c(3,5)) # null region left from hdi, with overlap
# 
# ropehdi(nullregion = c(1,6), credregion = c(3,5)) # null region encompassing hdi
# 
# ropehdi(nullregion = c(6,7), credregion = c(3,5)) # null region right from hdi and disjunct
# 
# ropehdi(nullregion = c(4,7), credregion = c(3,5)) # null region right from hdi with overlap
# 
# ropehdi(nullregion = c(4,4.5), credregion = c(3,5)) # null region encompassed by hdi







## END OF DOCUMENT
