#' @title Internal function newPayload
#' @description Create a list() object to hold data frames and table titles from other functions
#' @usage q.newPayload() 
#' @return A list called q.payload
#' @examples 
#' q.newPayload()
#' @export
#'
q.newPayload <- function(){
  if(!exists("q.payload")){
    q.payload <- list()
    
  } else if(exists("q.payload")){
    if(!class(q.payload) == "list")
      q.payload <- list()
    
  } else{
    q.payload <- q.payload
    
  }
  
  return(q.payload)
}


