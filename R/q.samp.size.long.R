#' @title Sample size for comparing means for longitudinal data
#' @aliases q.samp.size.long
#' @description Calculate sample size for comparing mean changes for longitudinal data involving 2 time points based on \code{powerMediation::ssLongFull} 
#' @usage q.samp.size.long(ref.mean=NULL, comp.mean=NULL, baseline.sd=NULL, 
#'                 endline.sd=NULL, dropout=0.2, arms=2, rho=0.5,
#'                 sig.level = 0.05 , power = 0.8)
#' @param ref.mean Mean for change in values within a treatment group
#' @param comp.mean Mean for change in values within the comparison group
#' @param baseline.sd Standard deviation for baseline values 
#' @param endline.sd 	Standard deviation for end-of-study/follow-up values 
#' @param dropout Dropout rate to be accounted for in sample size calculation. Default is 0.2
#' @param arms Number of arms in the planned study
#' @param rho 	Correlation coefficient between baseline and follow-up values within a treatment group. Default is 0.5
#' @param sig.level Significance level. Default is 0.05
#' @param power Power. Default is 0.8
#' @return A list containing the mean difference, mean ratio, number of completers per arm, total sample size per arm, total sample size for the study and number of study arms
#' @author Abdul Malik Sulley <asulley@uwo.ca> May 8, 2020
#' @seealso powerMediation::ssLongFull
#' @examples 
#' ref.mean <- 57.2
#' comp.mean <- 22.8
#' baseline.sd <- 21.1
#' endline.sd <- 32.2
#' q.samp.size.long(ref.mean, comp.mean, baseline.sd, endline.sd, dropout=0.2)
#' @export
#'
q.samp.size.long <- function(ref.mean=NULL, comp.mean=NULL, baseline.sd=NULL, endline.sd=NULL, dropout=0.2, arms=2, 
                          rho=0.5, sig.level = 0.05 , power = 0.8){
  
  
  mean_diff = ref.mean-comp.mean
  mean_ratio = round(ref.mean/comp.mean, 2)
  myn = powerMediation::ssLongFull(delta = mean_diff, sigma1 = baseline.sd, sigma2 = endline.sd, rho = rho, alpha = sig.level, power = power)
  n=ceiling(myn)
  n_adj = myn/(1-dropout)
  n_adj = ceiling(n_adj)
  
  
    completers.per.arm = n
    sample.size.per.arm = n_adj
    total.completers = n*arms
    total.sample.size = n_adj*arms
  
 
  q_ss <- list(
    "Completers per Arm" = completers.per.arm,
    "Total Completers" = total.completers,
    "Total per Arm (incl. dropout rate)" = sample.size.per.arm,
    "Total Sample Size (incl. dropout rate)" = total.sample.size,
    "Mean diff" = mean_diff,
    "Mean Ratio" = mean_ratio,
    "Number of Arms" = arms
  )
  
  structure(q_ss, class="power.htest")
  #return(q_ss)
  
}
