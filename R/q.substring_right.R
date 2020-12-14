#' @title Substring of a Character Vector from Right End of the String
#' @aliases q.substring_right
#' @description Extract a substring n characters from the right end of a string
#' @usage q.substring_right(x, n)
#' @param x string
#' @param n number of characters from the right end of the string to extract
#' @details Extract a substring n characters from the right end of a string. This is useful when traversing a vector, list or data frame with strings of different lengths.
#' @return A character vector of length n
#' @author Abdul Malik Sulley
#' @examples 
#' q.substring_right("twelve", 3)
#' # "lve"
#' @export

q.substring_right <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
