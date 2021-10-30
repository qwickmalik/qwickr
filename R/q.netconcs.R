#' @title Generate net analyte concentrations 
#' 
#' @aliases q.netconcs
#' 
#' @description Generate net analyte concentrations 
#' 
#' @usage q.netconcs(x, suffix="")
#' 
#' @param x Data frame. See example for format
#' @param suffix string/text to be included in the list item name 
#' 
#' @details Generate net analyte concentrations. 
#' @return Returns a data frame.
#' 
#' @author Abdul Malik Sulley <asulley@uwo.ca> Feb 12, 2021
#' 
#' @examples
#' x <- pkdata
#' q.netconcs(x)
#' 
#' @import crayon
#' @export
#'

q.netconcs <- function(x, suffix=""){
  # 
  lowest <- min(na.omit(x$TIME))
  lowest <- unique(lowest)
  cat("Min Time: ", lowest)
  time0concs <- x[x$TIME == lowest,]
  # time0concs <- subset(x$TIME == lowest)
  # print(head(time0concs))
  
  time0concs$CONC0 <- time0concs$CONC
  x2 <- merge(x=x, y = time0concs[,c('SUBJECTNUM', 'PERIOD', 'ANALYTE', 'CONC0')], by=c("SUBJECTNUM", "PERIOD", "ANALYTE"), all.x = TRUE )
  
  x2$VALUEDIFF <- x2$CONC - x2$CONC0
  x3 <- x2[order(x2$SUBJECTNUM, x2$SEQ, x2$PERIOD, x2$TIME, x2$ANALYTEN),] 
  
  
  cat(green("\u221a Net concentrations generated"), "\n")
  return(x3)
  
}

