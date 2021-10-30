#' @title LOCF Imputation
#' 
#' @aliases q.locf
#' 
#' @description Perform "last observation carried forward" imputation on a dataset. If the first measurement is missing, the function also performs "first observation carried backward" imputation.
#' 
#' @usage q.locf(db, sliceby=c())
#' 
#' @param db Data frame with missing values
#' @param sliceby Fields by which the data should be split before imputation. This ensures that each imputation is only based on the same subject's data
#' 
#' @return A data frame
#' 
#' @author Abdul Malik Sulley <asulley@uwo.ca> April 5, 2020
#' 
#' @note Dataset needs to be ordered by subject ID and time, and any other characteristics. This ensures that only the previous measurement is used for imputation.
#' 
#' @seealso zoo::na.locf
#' @importFrom stringr %>%
#' @examples 
#' mybeaver <- beaver1
#' #order by day and time
#' mybeaver <- mybeaver[order(mybeaver$day, mybeaver$time),] 
#' #make some data missing
#' mybeaver$temp[c(6, 10, 22, 55, 57)] <- NA
#' q.locf(mybeaver, sliceby="day")
#' @export
#'
q.locf <- function(db, sliceby=c()){
  
  db2 <- db %>%
    purrrlyr::slice_rows(sliceby) %>%
    purrrlyr::by_slice(function(x) { 
      zoo::na.locf(zoo::na.locf(x, na.rm = F), fromLast=T, na.rm = F) },
      .collate = "rows") #locf both directions
  
  return(db2)
}


