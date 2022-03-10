#' @title Generate PK concentration listings
#' 
#' @aliases qwickr.pkconcs
#' 
#' @description Generate PK concentration listings
#' 
#' @usage qwickr.pkconcs(db, suffix="")
#' 
#' @param db Data frame. See example for format
#' @param suffix string/text to be included in the list item name 
#' 
#' @details Generates PK concentration listings according to analyte and study group. It generates tables for both raw concentrations and change from baseline/pre-dose. 
#' @return Returns a list of data frames.
#' 
#' @author Abdul Malik Sulley <asulley@uwo.ca> Feb 8, 2021
#' 
#' @examples
#' db <- pkdata
#' 
#' qwickr.pkconcs(db)
#' @import crayon
#' @export
#'

qwickr.pkconcs <- function(db, suffix=""){
  tictoc::tic()
  q.payload <- q.newPayload()
  nl = "\n"  #new line
  
  
  conc.db <- db[c("SUBJECTNUM", "ANALYTE", "ANALYTEN", "SEQ", "PERIOD", "TRT", "TIME", "CONC")]
  names(conc.db)[names(conc.db) == "TIME"] <- "variable"
  names(conc.db)[names(conc.db) == "CONC"] <- "value"
  conc.cast <- reshape2::dcast(conc.db, SUBJECTNUM + ANALYTE + ANALYTEN + SEQ + PERIOD + TRT ~ variable)
  #print(head(conc.cast))
  trt <- unique(conc.cast$TRT)
  metabolites <- unique(conc.cast$ANALYTEN)
  analytes <- unique(conc.cast$ANALYTE)
  #names(conc.cast)
  iauc.comb <- data.frame()
  
  for (m in metabolites) {
    #cat("Metabolite: ", m, nl)
    #cat("Analytes: ", analytes, nl)
    analyte <- analytes[as.numeric(m)]
    analyte <- paste(analyte, collapse=" ")
    cat("Analyte: ", analyte, nl)
    output.conc1 <- conc.cast[conc.cast$ANALYTEN == m, ]
    
    for (t in trt) {
      output.conc2 <- output.conc1[output.conc1$TRT == t, ]
      output.conc <- output.conc2[,c(7:ncol(output.conc2))]
      output.colsd <- apply(output.conc, 2, sd, na.rm = T)
      output.colsd <- as.data.frame(output.colsd)
      #output.colmean <- colMeans(output.conc[c(3:14)], na.rm = T)
      output.colmean <- apply(output.conc, 2, mean, na.rm = T)
      output.colmean <- as.data.frame(output.colmean )
      output.meansd <- cbind.data.frame(output.colmean, output.colsd)
      names(output.meansd)[1] <- "Mean"
      names(output.meansd)[2] <- "SD"
      output.meansd$CV <- (output.meansd$SD/output.meansd$Mean) *100
      
      t.meansd <- as.data.frame(t(output.meansd))
      t.matrix <- output.conc2[c(1:3),c(1:6)]
      t.matrix[,c(1:6)] <- ""
      t.matrix[1,6] <- "Mean"
      t.matrix[2,6] <- "SD"
      t.matrix[3,6] <- "CV"
      t.meansdcv <- cbind.data.frame(t.matrix, t.meansd)
      
      output.meansdcv <- rbind.data.frame(output.conc2, t.meansdcv)
      output.meansdcv2 <- output.meansdcv %>% dplyr::mutate_if(is.numeric, ~round(., digits=3))
      
      ## Export main output
      # if(exportpath == "" | is.na(exportpath) | is.null(exportpath)){ exportpath <- paste0(getwd(), "/") } else { exportpath <- paste0(getwd(), "/", exportpath, "/") }
      
      # write.csv(output.meansdcv2, file = paste0(exportpath, analyte, "_Conc_Group_",suffix, "_", t, "_", ".csv"))
      
      q.payload[[paste("PK_concs", suffix, analyte, t, sep = "_")]] <- output.meansdcv2
      
      iauc.db <- output.conc2
      times=unique(db$TIME)
      
      for (ti in times) {
        newvar <- paste0("Incre", ti)
        iauc.db[, newvar] <-  iauc.db[, toString(ti)] - iauc.db[, toString(times[1])]
      }
      
      # write.csv(iauc.db, file = paste0(exportpath, analyte, "_Conc_iAUC_Group_",suffix, "_", t, "_", ".csv"))
      
      iauc.comb <- rbind.data.frame(iauc.comb, iauc.db)
    }
  }
  
  
  q.payload[[paste("iauc.comb", suffix, sep = "_")]] <- iauc.comb
  
  
  cat(green("\u221a PK concentration tables generated"), "\n")
  tictoc::toc(); 
  return(q.payload)
  
}

