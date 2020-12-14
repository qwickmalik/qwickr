#' @title Round numbers up or down
#' @aliases q.n_decimals
#' @description Round numbers up or down to specified decimal places and return a string
#' @usage q.n_decimals(x, k)
#' @param x Number
#' @param k Number of decimal places
#' @return Returns a string
#' @author Abdul Malik Sulley
#' @examples 
#' q.n_decimals(2.239849223, 3)
#' ## 2.240
#' @export

q.n_decimals <- function(x, k) {
    if(!is.null(x)){
      trimws(format(round(x, k), nsmall=k))
    }
  }
