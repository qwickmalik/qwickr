#' @title Export data frames and text to a Word document
#' @aliases q.write.to.word
#' @description Write data frames and text to a Word document. Content to be exported must be stored in a \code{list()}
#' @usage q.write.to.word(q.payload, exportpath="", docname="", colwidths=c()) 
#' @param q.payload A \code{list()}
#' @param exportpath Path relative to the working directory where exported files will be saved e.g. "/OUTPUT". Always begin with a backslash and end without one. If left empty, file will be exported to the working directory.
#' @param docname \code{String} - File name for the Word document to be created
#' @param colwidths A vector of 3 lengths (in cm) specifying the width of the first, middle and last column(s) respectively. If the table to be exported has only 2 columns, the first and last elements of the vector are used. If empty, automatic widths will be applied based on column content.
#' @return A Word document
#' @examples 
#' groupA <- c(20, 30)
#' groupB <- c(12, 37)
#' pValue <- c(0.033, NA)
#' db <- matrix(data=c(groupA, groupB, pValue), nrow = 2, ncol = 3)
#' q.write.to.word(db, docname="Output.doc")
#' @import tictoc
#' @import crayon
#' @export
#' 
q.write.to.word <- function(q.payload, exportpath="", docname="", colwidths=c()){
  
  
  if(!exists("q.payload")){
    stop("Cannot find q.payload.")
  } else if(docname == "" | is.null(docname)){
    stop("Please provide a name for the Word document to be created.")
  }
  tic()
  # exp♥rt path
  if(exportpath != ""){ 
      exportpath <- paste0(getwd(), "/", exportpath, "/") 
    } else { 
      exportpath <- paste0(getwd(), "/") 
      }
  
  if(length(colwidths) == 3){
    fixedwidth <- T
    firstcol <- colwidths[1]/2.54;
    othercol <- colwidths[2]/2.54; #divide cm by 2.54 to give inches
    lastcol <- colwidths[3]/2.54; 
    
  } else {
    fixedwidth <- F
  }
  
  
  filename = paste0(docname, ".doc")
  myoutput.doc <- rtf::RTF(paste0(exportpath, filename))
  mypayload <- q.payload
  
  for (each_item in names(mypayload)) {
    print(each_item)
    rtf::addNewLine(myoutput.doc)
    rtf::addHeader(myoutput.doc, title = each_item)
    
    if (class(mypayload[[each_item]]) == "character") {
      mytext <- mypayload[[each_item]]
      
      rtf::addText(myoutput.doc, mytext, bold=FALSE, italic=FALSE)
      
    } else if (class(mypayload[[each_item]]) == "data.frame") {
      mytable <- mypayload[[each_item]]
      colnumb <- ncol(mytable)
      if(colnumb > 2 & isTRUE(fixedwidth)){
        othercols <- rep(othercol, colnumb-2)
        mycolwidth <- c(firstcol, othercols, lastcol)
      }
      
      if(isTRUE(fixedwidth)){
        #print(mycolwidth)
        #addTable(myoutput.doc, mytable, col.widths=mycolwidth, col.justify="C", font.size=8)
        rtf::addTable(myoutput.doc, mytable, col.widths=mycolwidth,
                      col.justify="C", font.size=8)
      } else {
        mytable <- mypayload[[each_item]]
        rtf::addTable(myoutput.doc, mytable, col.justify="C", font.size=8)
      }
    }
    
  }
  cat(green("Export Path: " %+% exportpath))
  rtf::done(myoutput.doc)
  done <- toc()
  elapsed_time <- round(done$toc - done$tic, 2)
  cat(green(filename %+% " successfully created."), "\n")
}
