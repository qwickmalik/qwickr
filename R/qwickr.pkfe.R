#' @title Generate Fixed Effects Tables for PK Parameters
#' 
#' @aliases qwickr.pkfe
#' 
#' @description Run descriptive summaries and generate fixed effects tables
#' 
#' @usage qwickr.pkfe(x, outcomevar, groupvar,  design="", assume.normal.dist=F)
#' 
#' @param x Data frame
#' @param outcomevar Name of outcome variable \code{(string)}
#' @param groupvar Name of the group variable \code{(string)}
#' @param design specify the study design. Options: c("parallel", "crossover"). Default: "crossover"
#' @param assume.normal.dist assume that the data is normally distributed? (T/F) Default: `FALSE`, the Shapiro Wilk test will be used to assess normality of the data. Data will be ranked if not normally distributed.
#' @details Uses linear mixed effects model to compare PK parameters between groups and generate fixed effects tables as per [Health Canada](https://www.canada.ca/content/dam/hc-sc/documents/services/drugs-health-products/drug-products/applications-submissions/guidance-documents/bioavailability-bioequivalence/conduct-analysis-comparative.pdf) format.
#' @return Returns a data frame of counts and percentages for each study arm and an associated p-value for each study time point in a repeated measures design.
#' 
#' @author Abdul Malik Sulley <asulley@uwo.ca> May 7, 2020
#' 
#' 
#' @seealso stats::fisher.test(), q.write.to.word, utils::write.csv
#' @examples 
#' x <- qwickr.pkparams(pkdata)
#' pkfe.output <- qwickr.pkfe(x[[1]], outcomevar = "AUC", groupvar = "GROUPING")
#' @import tictoc
#' @import crayon
#' @export
#'

qwickr.pkfe <- function(x, outcomevar, groupvar, design="crossover", assume.normal.dist=F){
  tic()
  ## Test for normality
  normality <- function(x){
    cat(paste0("Plain:   ",shapiro.test(x)$p.value,"\n"))
    cat(paste0("Log:     ",shapiro.test(log(x+1))$p.value,"\n"))
    cat(paste0("SQRT:    ",shapiro.test(sqrt(x))$p.value,"\n"))
    cat(paste0("Squared: ",shapiro.test((x^2))$p.value,"\n"))
    
    #plots
    c <- graphics::hist(x)
    a <- qqnorm(x)
    b <- qqline(x) 
    
    
    
  }
  
  f <- function(x) {
    c(N = round(length(na.omit(x)), 0), MED = round(arsenal::medianrange(x, na.rm = TRUE), 2), MN = round(mean(x, na.rm = TRUE), 2), SD = round(sd(x, na.rm = TRUE), 2))
  }
  
  q.payload <- q.newPayload()
  w <- x
  w["OUTCOME"] <- w[outcomevar]
  w["GROUPING"] <- w[groupvar]
  # w["VISITNUMBER"] <- w[timevar]
  # vvists <- w$VISITNUMBER
  # vvists1 <- sort(unique(vvists))
  # visitnumbers <- vvists1[-1]
  # baselinevisit <- vvists[1]
  # cat("Pre-dose time point: ", baselinevisit, "\n")
  # cat("Post-dose time points: ", visitnumbers, "\n")
  
  
  ggroups <- w$GROUPING
  ggroups <- unique(ggroups)
  grps <- length(ggroups)
  group.names <- ggroups
  
  metabolites <- unique(w$ANALYTEN)
  groupings <- unique(w$TRT)
  analytes <- unique(w$ANALYTE)
  # cat("Metabolites", metabolites, "\n")
  cat("Analytes", analytes, "\n")
  subjects <- unique(w$SUBJECTNUM)
  cat("Subjects: ", subjects, "\n")
  
  # 
  # viz <- NA
  # if(isTRUE(baselinevisit == visitnumbers)){
  #   viz <- baselinevisit
  # } else {
  #   viz <- c( baselinevisit, visitnumbers )
  # }
  # 
  #* Descriptives ####
  #summarize data by visit
  cat( "\n", "\n", "\n", "\n", "# ------- ------- DESCRIPTIVE STATS  ------- -------- #", "\n")
  for(m in metabolites ){
    
    u <- w[which( w$ANALYTEN == m ),]
    
    output <- setNames(data.frame(matrix(ncol = grps + 3, nrow = 0)), c("Analyte", "PK Parameter", "Statistics Parameter", group.names) )
    
    # for (i in viz) {
      # u1 <- u[which( u$VISITNUMBER == i ),]
      u1 <- u
      cat("Metabolite: ", m, "\n")
      analyte <- analytes[as.numeric(m)]
      cat("Analyte: ", analyte, "\n")
      
      mysumm <- with( u1, aggregate(OUTCOME, by=list(GROUP = TRT), FUN = f) )
      # cat("# ------- ------- VISIT ",i," ------- -------- #", "\n")
      print(mysumm)
      
      s.test <- shapiro.test(u1$OUTCOME)$p.value
      normality(u1$OUTCOME)
      cat("Sums Shapiro Test 1: ", s.test, "\n")
      
      
      #* Fixed Effects ####
      if(isTRUE(assume.normal.dist)){
        if(length(ggroups) == 1){
          p.val <- NA
        } else {
          if(design=="parallel"){
            mysumm.m <-nlme::lme(OUTCOME ~ TRT, random=~1|SUBJECTNUM, data = u1, na.action = na.omit )
            
          } else if(design=="crossover"){
            mysumm.m <-nlme::lme(OUTCOME ~ SEQ + PERIOD + TRT, random=~1|SUBJECTNUM, data = u1, na.action = na.omit )
          } else {
            stop("Please specify study design using one of these options: 'parallel' or 'crossover'")
          }
          print(summary(mysumm.m))
          result <- nlme::anova.lme(mysumm.m)
          result$`p-value` <- q.n_decimals(result$`p-value`, 3)
          result$`F-value` <- q.n_decimals(result$`F-value`, 2)
        }
        
      } 
      else {
        if(s.test < 0.05){ 
          
          if(length(ggroups) == 1){
            p.val <- NA
            
          } else {
            
            u1$OUTCOME2 <- rank(u1$OUTCOME, ties.method="average")
            
            if(design=="parallel"){
              mysumm.m <- nlme::lme(OUTCOME2 ~ TRT, random=~1|SUBJECTNUM, data = u1, na.action = na.omit )
            } else if(design=="crossover"){
              mysumm.m <- nlme::lme(OUTCOME2 ~ SEQ + PERIOD + TRT, random=~1|SUBJECTNUM, data = u1, na.action = na.omit )
            } else {
              stop("Please specify study design using one of these options: 'parallel' or 'crossover'")
            }
            print(summary(mysumm.m))
            result <- nlme::anova.lme(mysumm.m)
            result$`p-value` <- paste0(q.n_decimals(result$`p-value`, 3), "(r)")
            result$`F-value` <- q.n_decimals(result$`F-value`, 2)
          } 
          
          
        } else {
          if(length(ggroups) == 1){
            p.val <- NA
          } else {
            if(design=="parallel"){
              mysumm.m <-nlme::lme(OUTCOME ~ TRT, random=~1|SUBJECTNUM, data = u1, na.action = na.omit )
            } else if(design=="crossover"){
              mysumm.m <-nlme::lme(OUTCOME ~ SEQ + PERIOD + TRT, random=~1|SUBJECTNUM, data = u1, na.action = na.omit )
            } else {
              stop("Please specify study design using one of these options: 'parallel' or 'crossover'")
            }
            print(summary(mysumm.m))
            result <- nlme::anova.lme(mysumm.m)
            result$`p-value` <- q.n_decimals(result$`p-value`, 3)
            result$`F-value` <- q.n_decimals(result$`F-value`, 2)
          }
          
        }
        
      }
      
      
      #* Output things ####
      #means & SDs
      ms.output.list <- list()
      ms.output.list[["Analyte"]] <- analyte
      ms.output.list[["PK Parameter"]] <- outcomevar
      ms.output.list[["Statistics Parameter"]] <- "Mean (SD)"
      for(j in 1:grps) {
        if(outcomevar == "AHALFLIFE"){
          ms.output.list[[group.names[j]]] <- paste0( format(mysumm$x[j,5], scientific=T), " \u00B1 ", format(mysumm$x[j,6], scientific=T), " (", mysumm$x[j,1], ")" )
        } else {
          ms.output.list[[group.names[j]]] <- paste0( q.n_decimals(mysumm$x[j,5], 2), " \u00B1 ", q.n_decimals(mysumm$x[j,6], 2), " (", mysumm$x[j,1], ")" )
        }
        
      }
      output[nrow(output) + 1,] = ms.output.list
      
      #medians & ranges
      mr.output.list <- list()
      mr.output.list[["Analyte"]] <- ""
      mr.output.list[["PK Parameter"]] <- ""
      mr.output.list[["Statistics Parameter"]] <- "Median (Min-Max)"
      for(k in 1:grps) {
        if(outcomevar == "AHALFLIFE"){
          mr.output.list[[group.names[k]]] <- paste0( format(mysumm$x[k,2], scientific=T), " (", format(mysumm$x[k,3], scientific=T), " to ", format(mysumm$x[k,4], scientific=T), ")" )
        } else {
          mr.output.list[[group.names[k]]] <- paste0( q.n_decimals(mysumm$x[k,2], 2), " (", q.n_decimals(mysumm$x[k,3], 2), " to ", q.n_decimals(mysumm$x[k,4], 2), ")" )
        }
      }
      output[nrow(output) + 1,] = mr.output.list
      
      #CV
      cv.output.list <- list()
      cv.output.list[["Analyte"]] <- ""
      cv.output.list[["PK Parameter"]] <- ""
      cv.output.list[["Statistics Parameter"]] <- "CV"
      for(l in 1:grps) {
        cv <- (mysumm$x[l,6] / mysumm$x[l,5]) * 100
        cv.output.list[[group.names[l]]] <- paste0( q.n_decimals(cv, 2))
      }
      output[nrow(output) + 1,] = cv.output.list
      
      #fixed effects headings
      fe.output.list <- list()
      fe.output.list[["Analyte"]] <- "Effects"
      fe.output.list[["PK Parameter"]] <- "Num df*"
      fe.output.list[["Statistics Parameter"]] <- "Den df*"
      fe.output.list[["A"]] <- "F Value"
      fe.output.list[["B"]] <- "Prob > F***"
      output[nrow(output) + 1,] = fe.output.list
      
      
      #anova.object$numDF[2]
      #anova.object$denDF[2]
      #anova.object$`F-value`[2]
      #anova.object$`p-value`[2]
      if(design == "crossover"){
        #sequence effects
        se.output.list <- list()
        se.output.list[["Analyte"]] <- "Sequence"
        se.output.list[["PK Parameter"]] <- result$numDF[2]
        se.output.list[["Statistics Parameter"]] <- result$denDF[2]
        #se.output.list[["A"]] <- q.n_decimals(result$`F-value`[2], 2)
        #se.output.list[["B"]] <- q.n_decimals(result$`p-value`[2], 3)
        se.output.list[["A"]] <- result$`F-value`[2]
        se.output.list[["B"]] <- result$`p-value`[2]
        output[nrow(output) + 1,] = se.output.list
        
        #period effects
        pe.output.list <- list()
        pe.output.list[["Analyte"]] <- "Period"
        pe.output.list[["PK Parameter"]] <- result$numDF[3]
        pe.output.list[["Statistics Parameter"]] <- result$denDF[3]
        #pe.output.list[["A"]] <- q.n_decimals(result$`F-value`[3], 2)
        #pe.output.list[["B"]] <- q.n_decimals(result$`p-value`[3], 3)
        pe.output.list[["A"]] <- result$`F-value`[3]
        pe.output.list[["B"]] <- result$`p-value`[3]
        output[nrow(output) + 1,] = pe.output.list
        
        #form effects
        fo.output.list <- list()
        fo.output.list[["Analyte"]] <- "Form"
        fo.output.list[["PK Parameter"]] <- result$numDF[4]
        fo.output.list[["Statistics Parameter"]] <- result$denDF[4]
        #fo.output.list[["A"]] <- q.n_decimals(result$`F-value`[4], 2)
        #fo.output.list[["B"]] <- q.n_decimals(result$`p-value`[4], 3)
        fo.output.list[["A"]] <- result$`F-value`[4]
        fo.output.list[["B"]] <- result$`p-value`[4]
        output[nrow(output) + 1,] = fo.output.list
        
        if(q.substring_right(result$`p-value`[4], 3) == "(r)"){
          #footnote
          fn.output.list <- list()
          fn.output.list[["Analyte"]] <- "(r) indicates data was ranked before conducting ANOVA"
          fn.output.list[["PK Parameter"]] <- ""
          fn.output.list[["Statistics Parameter"]] <- ""
          fn.output.list[["A"]] <- ""
          fn.output.list[["B"]] <- ""
          output[nrow(output) + 1,] = fn.output.list
        }
      } 
      else if(design == "parallel"){
        #form effects
        se.output.list <- list()
        se.output.list[["Analyte"]] <- "Form"
        se.output.list[["PK Parameter"]] <- result$numDF[2]
        se.output.list[["Statistics Parameter"]] <- result$denDF[2]
        #se.output.list[["A"]] <- q.n_decimals(result$`F-value`[2], 2)
        #se.output.list[["B"]] <- q.n_decimals(result$`p-value`[2], 3)
        se.output.list[["A"]] <- result$`F-value`[2]
        se.output.list[["B"]] <- result$`p-value`[2]
        output[nrow(output) + 1,] = se.output.list
        
        if(q.substring_right(result$`p-value`[2], 3) == "(r)"){
          #footnote
          fn.output.list <- list()
          fn.output.list[["Analyte"]] <- "(r) indicates data was ranked before conducting ANOVA"
          fn.output.list[["PK Parameter"]] <- ""
          fn.output.list[["Statistics Parameter"]] <- ""
          fn.output.list[["A"]] <- ""
          fn.output.list[["B"]] <- ""
          output[nrow(output) + 1,] = fn.output.list
        }
      }
      
      
      
    # }
    
    # write.csv(output, file = paste(output_path,"/", analyte,"/", outcomevar, "_", popul, "_", filesuffix, "_", analyte, "_Sum.csv", sep=""))
    
    output.title <- paste0("Summary of ", analyte, " ", outcomevar," analysis for participants in the [XXX] population")
    q.payload[[paste("TITLE", outcomevar, analyte, sep = "_")]] <- output.title
    q.payload[[paste(outcomevar, analyte, sep = "_")]] <- output
  }
  
  #write.csv(output, file = paste(output_path,"/",filesuffix,"/", analyte,"/", outcomevar, "_", popul, "_Sum.csv", sep=""))
  
  
  cat(green("\u221a PK fixed effects table generated "), "\n", "\n", "\n")
  toc()
  return(q.payload)
  
}
