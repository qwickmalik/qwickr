#' @title Calculate Bioequivalence with 90\% CI
#' 
#' @aliases qwickr.pkbe
#' 
#' @description Calculate bioequivalence along with 90\% confidence intervals and coefficient of variation 
#' 
#' @usage qwickr.pkbe(x, outcomevars=c(), design="crossover")
#' 
#' @param x Data frame. See example for format
#' @param outcomevars vector of variable names/column titles based on which BE will be calculated e.g.\code{c("LOGAUC", "LOGCMAX")}
#' @param design specify the study design. Options: c("parallel", "crossover"). 
#' 
#' @details Calculate bioequivalence along with 90\% confidence intervals and coefficient of variation 
#' @return Returns a list of data frames - one data frame per analyte.
#' 
#' @author Abdul Malik Sulley <asulley@uwo.ca> Feb 18, 2021
#' 
#' @examples
#' x <- qwickr.pkparams(pkdata)
#' pkbe.output <- qwickr.pkbe(x[[1]], outcomevars = c("LOGAUC", "LOGCMAX"), design = "crossover")
#' print(pkbe.output)
#' @import crayon
#' @export
#'

qwickr.pkbe <- function(x, outcomevars=c(), design="crossover"){
    sitecontrol=F
    q.payload <- q.newPayload()
    be_field_names <- c("REFERENCE", "ANALYTEN", "FIELD", "ESTIMATE", "LOWER", "UPPER", "CV")
    db <- as.data.frame(x)
    
    groups <- sort(unique(db$GROUPING))
    group2 <- paste0("GROUPING", groups[2])
    print(group2)
    metabolites <- unique(db$ANALYTEN)
    analytes <- unique(db$ANALYTE)
    
    for (m in metabolites) {
      w <- db[db$ANALYTEN == m,]
      cat("Analyte", m, "\n")
      analyte <- analytes[as.numeric(m)]
      be.output <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), be_field_names )
      
      
      for (outcomevar in outcomevars) {
        w["FIELD"] <- w[outcomevar]
        cat("PK parameter: ", outcomevar, "\n")
        
        if(!isTRUE(sitecontrol)){
          if(design == "parallel"){
            OAM <- nlme::lme(FIELD ~ GROUPING, random=~1|SUBJECTNUM, data=w, 
                       control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), 
                       na.action = na.omit)
          } else if(design == "crossover"){
            OAM <- nlme::lme(FIELD ~ GROUPING + PERIOD + SEQ, random=~1|SUBJECTNUM, data=w, 
                       control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), 
                       na.action = na.omit)
          } else {
            stop("Please specify study design using one of these options: 'parallel' or 'crossover'")
          }
          
        } else {
          if(design == "parallel"){
            OAM <- nlme::lme(FIELD ~ GROUPING + SITE, random=~1|SUBJECTNUM, data=w, 
                       control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), 
                       na.action = na.omit)
          } else if(design == "crossover"){
            OAM <- nlme::lme(FIELD ~ GROUPING + PERIOD + SEQ + SITE, random=~1|SUBJECTNUM, data=w, 
                       control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), 
                       na.action = na.omit)
          } else {
            stop("Please specify study design using one of these options: 'parallel' or 'crossover'")
          }
          
          
        }
        # print(summary(OAM))
        coOAM <- nlme::fixef(OAM) #coefficients. same as L1$coefficients$fixed
        
        # print(OAM)
        
        SEOAM <- sqrt(stats::vcov(OAM)[group2, group2]) #SE for treatment. same as sqrt(L1$varFix["trtaT","trtaT"])
        dfOAM <- OAM$fixDF$terms[names(OAM$fixDF$terms)=="GROUPING"] #df for treatment
        tdOAM <- qt(.05,dfOAM) #t-value to use in calculating 90% CI
        
        
        
        # A as reference
        be.estimate <- 100*exp(coOAM[group2])
        be.lower <- 100*exp(coOAM[group2]+tdOAM*SEOAM)
        be.upper <- 100*exp(coOAM[group2]-tdOAM*SEOAM)
        be.cv <- 100*sqrt(exp(as.numeric(nlme::VarCorr(OAM)["Residual","Variance"]))-1)
        
        # B as reference
        be.estimatex <- 100*exp(-coOAM[group2])
        be.lowerx <- 100*exp(-(coOAM[group2]-tdOAM*SEOAM))
        be.upperx <- 100*exp(-(coOAM[group2]+tdOAM*SEOAM))
        be.cvx <- 100*sqrt(exp(as.numeric(nlme::VarCorr(OAM)["Residual","Variance"]))-1)
        
        
        
        be.output.list <- list("A", analyte, outcomevar, be.estimate, be.lower, be.upper, be.cv)
        be.output[nrow(be.output) + 1,] = be.output.list
        
        be.output.list2 <- list("B", analyte, outcomevar, be.estimatex, be.lowerx, be.upperx, be.cvx)
        be.output[nrow(be.output) + 1,] = be.output.list2
      }
      # if(!isTRUE(sitecontrol)){
      #   write.csv(be.output, file = paste0(output_path_main, popn, "/", analyte, "/BE_", outcomevar, filesuffix,"_", fileidentifier, ".csv"))
      # } else {
      #   write.csv(be.output, file = paste0(output_path_main, popn, "/", analyte, "/SENSITIVITY/", "BE_", outcomevar, filesuffix,"_", fileidentifier, ".csv"))
      # }
      
      
      # q.payload[[paste("BE", outcomevar, analyte, sep = "_")]] <- be.output
      q.payload[[paste("BE", analyte, sep = "_")]] <- be.output
      
    }
    

  cat(green("\u221a Bioequivalence tables generated"), "\n")
  return(q.payload)
  
}

