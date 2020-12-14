#' @title Internal function: Format table headers 
#' @aliases q.rename_headers
#' @description Format table headers depending on the type of analysis being conducted. 
#' @usage q.rename_headers(x, type="default")
#' @param x data frame
#' @param type Type of analysis. Specify one of these options: \code{c("longitudinal", "categorical", "default")}
#' @details For categorical variables, adds "n (%)" to headers. For Continuous variables, adds the following "Mean+-SD (n)", "Median (Min - Max)", "Within Group P-value"
#' @return Returns a tibble of results
#' @author Abdul Malik Sulley
#' @examples 
#' df.header <- c("#", "Group A", "Group B", "p.val")
#' row1 <- c("Day 7", 20, 50, 0.040)
#' row2 <- c("Day 14", 3, 12, 0.010)
#' df <- rbind.data.frame(row1, row2)
#' names(df) <- df.header
#' q.rename_headers(df)
#' @export


q.rename_headers <- function(x, type="default"){
  #type options: c("longitudinal", "categorical", "default")
  for (t in names(x)) {
    if(t == "#") { 
      if(type == "categorical"){
        names(x)[names(x) == t] <- paste0("Response/Study Timepoint") 
      } else {
        names(x)[names(x) == t] <- paste0("Study Timepoint") 
      }
      
      
    } else if(substring(t, 1, 5) %in% c("Group", "Treat", "Seque", "Inter", "Produ", "Formu") ) { 
      
      if(type == "default"){
        names(x)[names(x) == t] <- paste0(t, "\n","Mean ", "\u00B1", " SD (n)", "\n", "Median (Min - Max)") 
        
      } else if(type == "longitudinal") {
        names(x)[names(x) == t] <- paste0(t, "\nMean ", "\u00B1", " SD (n) \nMedian (Min - Max) \nWithin Group P-value") 
        
      } else if(type == "categorical"){
        names(x)[names(x) == t] <- paste0(t, "\n", " n (%)") 
        
      }
      
    } else if(t == "p.val" | t == "p-value") { 
      names(x)[names(x) == t] <- paste0("Between", "\n", "Groups", "\n", "P-value")
      
    }
  }
  
  return(x)
}

