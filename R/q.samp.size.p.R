#' @title Sample size for comparing proportions
#' @aliases q.samp.size.p
#' @description Calculate sample size for comparing proportions based on \code{pwr::pwr.2p.test}
#' @usage q.samp.size.p(p1=NULL, p2=NULL, dropout=0.2, arms=2, 
#'                sig.level = 0.05 , power = 0.8, alternative = "two.sided")
#' @param p1 Proportion for the product/intervention of interest
#' @param p2 Proportion for comparator/placebo
#' @param dropout Dropout rate to be accounted for in sample size calculation. Default is 0.2
#' @param arms Number of armes in the planned study
#' @param sig.level Significance level. Default is 0.05
#' @param power Power. Default is 0.8
#' @param alternative A charater string specifying the alternative hypothesis. Options:  "two.sided" (default), "greater" or "less" (see pwr.2p.test)
#' @return A list containing the ratio of proportions, number of completers per arm, total sample size per arm, total sample size for the study and number of study arms
#' @author Abdul Malik Sulley <asulley@uwo.ca> May 8, 2020
#' @seealso pwr.2p.test
#' @examples 
#' p1 <- 0.8
#' p2 <- 0.5
#' q.samp.size.p(p1, p2, dropout=0.2, arms=2, 
#'               sig.level = 0.05 , power = 0.8, alternative = "two.sided" )
#' @export
#'
q.samp.size.p <- function(p1=NULL, p2=NULL, dropout=0.2, arms=2, 
                          sig.level = 0.05 , power = 0.8, alternative = "two.sided"){
  
  h = pwr::ES.h(p1, p2)
  p_diff = p1-p2
  p_ratio = round(p1/p2, 2)
  myn = pwr::pwr.2p.test(n = NULL, h = h, sig.level = sig.level, power = power, alternative = alternative)
  n=ceiling(myn$n)
  n_adj = myn$n/(1-dropout)
  n_adj = ceiling(n_adj)
  
  completers.per.arm = n
  sample.size.per.arm = n_adj
  total.completers = n*arms
  total.sample.size = n_adj*arms
  

  n_note <- switch(alternative, greater = "One-sided test", less = "One-sided test", 
                 two.sided = "Two-sided test", NULL)

  q_ss <- list(
    "Completers per Arm" = completers.per.arm,
    "Total Completers" = total.completers,
    "Total per Arm (incl. dropout rate)" = sample.size.per.arm,
    "Total Sample Size (incl. dropout rate)" = total.sample.size,
    "Diff. of Proportions" = p_diff,
    "Ratio of Proportions" = p_ratio,
    "Note" = n_note,
    "Number of Arms" = arms
  )
              
  structure(q_ss, class="power.htest")
  #return(q_ss)
}
