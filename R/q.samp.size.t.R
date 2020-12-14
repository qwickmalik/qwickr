#' @title Sample size for comparing means
#' @aliases q.samp.size.t
#' @description Calculate sample size for a difference in mean based on pwr.t.test from the pwr package
#' @usage q.samp.size.t(refmean=NULL, compmean=NULL, SD=NULL, dropout=0.2, arms=2, 
#'              sig.level = 0.05 , power = 0.8, type = "two.sample", 
#'              alternative ="two.sided")
#' @param refmean Mean for the product/intervention of interest
#' @param compmean Mean for comparator/placebo
#' @param SD Pooled standard deviation for calculation of Cohen's d
#' @param dropout Dropout rate to be accounted for in sample size calculation. Default is 0.2
#' @param arms Number of armes in the planned study
#' @param sig.level Significance level. Default is 0.05
#' @param power Power. Default is 0.8
#' @param type A character string specifying the type of test. Options: "one.sample", "two.sample" (default) or "paired" (see pwr.t.test)
#' @param alternative A charater string specifying the alternative hypothesis. Options:  "two.sided" (default), "greater" or "less" (see pwr.t.test)
#' @return A list containing the mean difference, mean ratio, number of completers per arm, total sample size per arm, total sample size for the study and number of study arms
#' @author Abdul Malik Sulley <asulley@uwo.ca> 22 April, 2020
#' @seealso pwr.t.test
#' @examples 
#' sds <- c(21.1, 32.2, 43.3)
#' ns <- c(40, 37, 39)
#' SD <- q.pooled_sd(sds, ns)
#' refmean <- 57.2
#' compmean <- 22.8
#' q.samp.size.t(refmean, compmean, SD, dropout=0.2, 
#' type = "two.sample", alternative ="two.sided", arms=2 )
#' @export
#'
q.samp.size.t <- function(refmean=NULL, compmean=NULL, SD=NULL, dropout=0.2, arms=2, 
                          sig.level = 0.05 , power = 0.8, type = "two.sample", 
                          alternative ="two.sided"){
  mean_diff = refmean-compmean
  mean_ratio = round(refmean/compmean, 2)
  d = mean_diff/SD
  myn = pwr::pwr.t.test(n = NULL , d = d , sig.level = sig.level , power = power, type = type, alternative = alternative)
  n=ceiling(myn$n)
  n_adj = myn$n/(1-dropout)
  n_adj = ceiling(n_adj)
  
  if(type == "one.sample"){
    completers.per.arm = n
    sample.size.per.arm = n_adj
    total.completers = n
    total.sample.size = n_adj
    
  } else if(type == "paired"){
    completers.per.arm = ceiling(myn$n/arms)*arms
    sample.size.per.arm = ceiling(((myn$n/(1-dropout))/arms)*arms)
    total.completers = ceiling(myn$n/arms)*arms
    total.sample.size = ceiling(((myn$n/(1-dropout))/arms)*arms)
    
  } else if(type == "two.sample"){
    completers.per.arm = n
    sample.size.per.arm = n_adj
    total.completers = n*arms
    total.sample.size = n_adj*arms
  }
  
  
  #q_ss <- list()
  #q_ss[["Number of Arms"]] <- arms
  #q_ss[["Mean diff"]] <- mean_diff
  #q_ss[["Mean Ratio"]] <- mean_ratio
  #q_ss[["Completers per Arm"]] <- completers
  #q_ss[["Total per Arm"]] <- sample.size.per.arm
  #q_ss[["Total Completers"]] <- total.completers
  #q_ss[["Total Sample Size"]] <- total.sample.size
  
  n_note <- switch(type, paired = "Sample size is number of pairs", 
                 two.sample = "Sample size is number in each arm/group", NULL)
  type_note <- paste("Primary analysis is ", 
                     switch(type, one.sample = "ONE-SAMPLE", 
                         two.sample = "TWO-SAMPLE", 
                         paired = "PAIRED"), 
                     "t test")
  #structure(list(Arms = arms,
  #               n = n, 
  #               d = d, 
  #               sig.level = sig.level, 
  #               power = power, 
  #               alternative = alternative, 
  #               note = n_note, 
  #               method = type_note), 
  #          class = "power.htest")
  
  q_ss <- list(
    "Completers per Arm" = completers.per.arm,
    "Total Completers" = total.completers,
    "Total per Arm (incl. dropout rate)" = sample.size.per.arm,
    "Total Sample Size (incl. dropout rate)" = total.sample.size,
    "Mean diff" = mean_diff,
    "Mean Ratio" = mean_ratio,
    "Note" = n_note,
    "Method" = type_note,
    "Number of Arms" = arms
  )
              
  structure(q_ss, class="power.htest")
  #return(q_ss)
}
