#' @title Pooled Standard Deviation
#' @aliases q.pooled_sd
#' @description Calculate the pooled standard deviation given a set of standard deviations and sample sizes
#' @usage q.pooled_sd(sds=c(), ns=c())
#' @param sds A vector containing standard deviations to be pooled
#' @param ns A vector of length \code{length(sds)}, containing the sample sizes matching each standard deviation in \code{sds}
#' @return Pooled standard deviation
#' @examples 
#' sds <- c(21.1, 32.2, 43.3)
#' ns <- c(40, 37, 39)
#' q.pooled_sd(sds, ns)
#' @export
#'
q.pooled_sd <- function(sds=c(), ns=c()){
  n = length(sds)
  n1 = length(ns)
  numerator <- c()
  if(n > 1 & !("TRUE" %in% is.na(ns))){
    if(n == n1){
      for (i in c(1:n)) {
        numerator[i] <- (sds[i]^2)*(ns[i]-1)
      }
      
      p_sd <- sqrt(sum(numerator, na.rm = T)/(sum(ns, na.rm = T)-n))
      p_sd <- round(p_sd, 3)
      cat("Pooled SD: ", p_sd, "\n")
      return(p_sd)
    } else {
      stop("sds and ns must have equal lengths")
    }
  } else {
    stop("sds is a vector of length > 1 and must not contain NA")
  }
}


ns <- c(40, NA, 39)
!("TRUE" %in% is.na(ns))
