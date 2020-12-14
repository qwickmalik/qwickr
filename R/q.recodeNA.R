#' @title Recode specified entries as NA (missing)
#' @aliases q.recodeNA
#' @description Recode specified values as missing (NA)
#' @usage q.recodeNA(x, pattern=c(-77, -99, -55, -33))
#' @param x A data frame
#' @param pattern A vector of numbers or character strings to be replaced with NA. Default: -77, -99, -55, -33
#' @return Returns a data frame
#' @author Abdul Malik Sulley <asulley@uwo.ca> May 11, 2020
#' @examples 
#' id <- c(1, 2, 3, 4, 5)
#' value <- c(25.2, 33.3, -99, 13.0, -77)
#' dframe <- data.frame(id, value)
#' q.recodeNA(x=dframe, pattern=c(-77, -99))
#' @export

q.recodeNA <- function(x, pattern=c(-77, -99, -55, -33)){
  #x[x == (-99) | x == (-77) | x == (-55) | x == (-33) ] <- NA
  for (i in pattern) {
    x[x == i ] <- NA
  }
  return(x)
}
