#' @title Summarize for categorical data
#' 
#' @aliases qwickr.cat
#' 
#' @description Run descriptive summaries for categorical data with the option to include hypothesis testing using the Chi Square test or Fisher's Exact test as appropriate.
#' 
#' @usage qwickr.cat(x, outcomevar="", groupvar="", timevar="", specgroups=NULL, 
#'           filesuffix="", t.title="", genpvalues=T, runpairwise=F, 
#'           exportfile=c(), exportpath="")
#' 
#' @param x Data frame
#' @param outcomevar Name of outcome variable \code{(string)}
#' @param groupvar Name of the group variable \code{(string)}
#' @param timevar Name of the time variable \code{(string)}
#' @param specgroups Do you want to compare a subset of groups within your dataset? Specify the group names here. For example, \code{specgroups=c("Group A", "Group B", "Group C")}. If NULL, all groups will be included in the analysis. 
#' @param filesuffix A string to be appended to the name of the returned .csv or .doc file
#' @param t.title Table title. Exported together with a dataframe of the output as a list. This list can then be exported to Word using the \code{q.write.to.word()} command 
#' @param genpvalues Should a Chi Square or Fisher's Exact test be performed? \code{T/F}. If true, the function will determine which test to perform
#' @param runpairwise Run pairwise comparisons for each unique group pair? \code{T/F}
#' @param exportfile Export the output to file? Options: \code{c(".csv", ".doc")}. See \code{q.write.to.word, stats::write.csv }
#' @param exportpath Path relative to the working directory where exported files will be saved e.g. "/OUTPUT". Always begin with a backslash and end without one. If left empty, file will be exported to the working directory.
#' @details The function will automatically determine whether to use Chi Square or Fisher's Exact test based on the nature of the data.
#' @return Returns a data frame of counts and percentages for each study arm and an associated p-value for each study time point in a repeated measures design.
#' 
#' @author Abdul Malik Sulley <asulley@uwo.ca> May 7, 2020
#' 
#' 
#' @seealso stats::fisher.test(), q.write.to.word, utils::write.csv
#' @examples 
#' group <- rep(c("A", "B"), 10)
#' gender <- rep(c(1,1,0,0), 5)
#' time <- rep(1, 10)
#' df <- data.frame(group, gender, time)
#' qwickr.cat(x=df, outcomevar="gender", groupvar = "group", timevar = "time")
#' @import tictoc
#' @import crayon
#' @export
#'

qwickr.cat <- function(x, outcomevar="", groupvar="", timevar="", specgroups=NULL,
                  filesuffix="", t.title="", genpvalues=T, runpairwise=F, exportfile=c(), exportpath=""){
  
  q.payload <- q.newPayload()
  cr_symb = "\u00BF"
  db <- x
  db["OUTCOME"] <- db[outcomevar]
  db["GROUPING"] <- db[groupvar]
  db["VISITNUMBER"] <- db[timevar]
  if(is.null(specgroups)){
    specgroups <- db$GROUPING
    specgroups <- unique(specgroups)
    grps <- length(specgroups)
  } else {
    grps <- length(specgroups)
  }
  grps
  specgroups <- sort(specgroups)
  grp.names <- specgroups 
  group.names <- as.vector(grp.names[1:grps])
  
  cat(green("+-------------------------------------------+"), "\n")
  cat(green(outcomevar %+% " (Categorical)") %+% "\n")
  cat(green("+-------------------------------------------+"), "\n")
  tic()
  
  cat("Study groups: ", group.names, "\n")
  output.pairwise <- output <- stats::setNames(data.frame(matrix(ncol = grps+2, nrow = 0)),
                                        c("Variable", group.names, "p-value") )
  
  p.val <- NULL
  vvists <- db$VISITNUMBER
  vvists <- unique(vvists)
  cat("Time points: ", vvists, "\n")
  
  for(vvist in vvists){
    db2 <- db[db$VISITNUMBER == vvist,]
    num.outcomes <- length(unique(db2$OUTCOME))
    
    result <- with(db2, table(OUTCOME, GROUPING))
    result.freq <- data.frame(result)
    
    if(isTRUE(genpvalues)){
      if(grps > 1 & num.outcomes > 1){
        result.summ <- summary(result)
        result.summ.p <- result.summ$p.value
        
        result.fisher <- try(stats::fisher.test(result, simulate.p.value=TRUE))
        #print(result.fisher)
        result.fisher.p <- result.fisher$p.value
      } else {
        result.fisher.p <- 1
        result.summ.p <- 1
      }
    } else {
      result.fisher.p <- -999
      result.summ.p <- -999
    }
    
    result.perc <- round(100* prop.table(result, 2), 1)
    result.percent <- data.frame(result.perc)
    result.percent$Freq <- ifelse(is.nan(result.percent$Freq), 0.00, 
                                  result.percent$Freq)
    
    #print(result.freq)
    #print(result.percent)
    outcome1 <- unique(db$OUTCOME)
    outcome1 <-  sort(outcome1)
    #Cat("Outcomes1: ", outcome1, "\n")
    
    outcome <- outcome1
    min.freq <- min(result.freq$Freq)
    
    #output the p-value
    p.val <- ifelse(min.freq <= 5, result.fisher.p, result.summ.p )
    p.val <- q.n_decimals(p.val, 3)
    p.val <- ifelse(p.val < 0.001, "<0.001", p.val)
    
    #main output
    p.output.list <- list()
    p.output.list[["Variable"]] <- paste0("Time ", vvist)
    for(k in 1:grps) {
      p.output.list[[group.names[k]]] <- " "
    }
    p.output.list[["p-value"]] <- p.val
    output[nrow(output) + 1, ] = p.output.list
    
    for(i in outcome){
      output.list <- list()
      output.list[["Variable"]] <- i
      for(j in 1:grps) {
        output.list[[group.names[j]]] <- 
          paste0(result.freq$Freq[result.freq$OUTCOME == i & 
                                    result.freq$GROUPING == grp.names[j]],
                 " (", q.n_decimals(result.percent$Freq[result.percent$OUTCOME == i &
                        result.percent$GROUPING == grp.names[j]], 2), "%)")
      }
      output.list[["p-value"]] <- " "
      
      output[nrow(output) + 1,] = output.list
    }
    
    #print(output)
    
    
    if(isTRUE(runpairwise)){
      output.list.pairwise <- list()
      output.list.pairwise[["Variable"]] <- paste0("Time ", vvist)
      grp.combs <- expand.grid(specgroups, specgroups)
      grp.combs <- grp.combs[!(grp.combs$Var1 == grp.combs$Var2), ]
      cat(silver("PAIRWISE COMPARISONS "), "\n")
      
      for (g in c(1:nrow(grp.combs))) {
        #cat(group.names[g] )
        pwdb <- db2[db2$GROUPING %in% c(as.character(grp.combs$Var1[g]),
                                        as.character(grp.combs$Var2[g])),]
        num.outcomes2 <- length(unique(pwdb$OUTCOME))
        pw.grps <- unique(pwdb$GROUPING)
        pw.grps <- sort(pw.grps)
        pw.grps <- toString(pw.grps)
        pw.grps <- gsub(",", " -", pw.grps)
        #cat("Number of outcomes", num.outcomes, "\n")
        result <- with(pwdb, table(OUTCOME, GROUPING))
        result.freq <- data.frame(result)
        #cat("Number of groups: ", pw.grps, "\n")
        #print(result.freq)
        
        if(num.outcomes2 > 1 | length(pw.grps) > 1){
          result.summ <- summary(result)
          result.summ.p <- result.summ$p.value
          
          result.fisher <- stats::fisher.test(result, simulate.p.value=T)
          #print(result.fisher)
          result.fisher.p <- result.fisher$p.value
        } else {
          result.fisher.p <- 1
          result.summ.p <- 1
        }
        
        min.freq1 <- min(result.freq$Freq)
        p.val <- ifelse(min.freq1 <= 5, result.fisher.p, result.summ.p )
        p.val <- q.n_decimals(p.val, 3)
        p.val <- ifelse(p.val < 0.001, "<0.001", p.val)
        p.val <- ifelse(p.val == 1, "1.000", p.val)
        cat(silver("Final pairwise p-value: " %+% as.character(p.val)), "\n", "\n")
        
        output.list.pairwise[[paste0(grp.combs$Var1[g], grp.combs$Var2[g])]] <- 
          paste0(p.val, " (", pw.grps, ")")
        
        
      }
      
      output.list.pairwise[["p-value"]] <- " "
      
      #output.pairwise[nrow(output.pairwise) + 1,] = output.list.pairwise
      output.pairwise <- data.frame(t(sapply(output.list.pairwise,c)))
    }
    
    #print(output.pairwise)
    cat(green("v ") %+% blue("TIME POINT " %+% as.character(vvist)), "\n");
    #cat("Time Point: ", vvist,"\n")
    cat(silver("  Outcome Responses: "%+% as.character(toString(outcome))), "\n")
    cat(silver("  Minimum count per cell: " %+% as.character(min.freq)), "\n")
    cat(silver("  Fisher's p-value: " %+% as.character(result.fisher.p)), "\n")
    cat(silver("  Chi Square p-value: " %+% as.character(result.summ.p)), "\n")
    cat(green("  Final p-value: " %+% as.character(p.val)), "\n")
    toc(); tic();
    cat("\n", "\n")
  }
  
  #Table footer
  header.output.list <- list()
  header.output.list[["#"]] <- outcomevar
  for(head_item in 1:grps) {
    header.output.list[[group.names[head_item]]] <- ""
  }
  header.output.list[["p-value"]] <- ""
  output[nrow(output) + 1,] = header.output.list
  output.pairwise[nrow(output.pairwise) + 1,] = header.output.list
  
  #Table header
  output <- q.rename_headers(output, type="categorical")
  
  ## Export main output
  if(!exportpath == ""){ exportpath <- paste0(getwd(), "/", exportpath, "/") } else { exportpath <- paste0(getwd(), "/") }
  
  #utils::write.csv(output, file = paste(exportpath, filesuffix, outcomevar,"_", grps, "_Cat.csv", sep=""))
  if(is.null(t.title) | is.na(t.title) | t.title == ""){
    t.title <- outcomevar
  }
    
  q.payload[[paste0("TITLE: ", outcomevar, filesuffix)]] <- t.title
  q.payload[[paste0(outcomevar, filesuffix)]] <- output
  #grandlist[["Main Output"]] <- q.payload
  
  
  #Prepare pairwis♥e comparison output
  if(isTRUE(runpairwise)){
    #utils::write.csv(output.pairwise, file = paste(exportpath, outcomevar, filesuffix, "_Cat_pairwise.csv", sep=""))
    output.pairwise.title <- paste0("Pairwise comparisons for: ", t.title)
    q.payload[[paste0("TITLE: ", outcomevar, filesuffix, "Pairwise")]] <- output.pairwise.title
    #q.payload.pairwise[[paste0(outcomevar, filesuffix, "Pairwise")]] <- output.pairwise
    q.payload[[paste0(outcomevar, filesuffix, "Pairwise")]] <- output.pairwise
    #grandlist[["Pairwise Comparisons"]] <- q.payload.pairwise
  }
  
  #Export to file 
  if(".csv" %in% exportfile){
    utils::write.csv(output, file = paste(exportpath, outcomevar, filesuffix, ".csv", sep=""))
    if(isTRUE(runpairwise)){
      utils::write.csv(output.pairwise, file = paste(exportpath, outcomevar, filesuffix, "_Pairwise.csv", sep=""))
    }
  }
  if(".doc" %in% exportfile){
    q.write.to.word(q.payload, exportpath=exportpath, docname=paste0(outcomevar, "_", filesuffix))
  }
  
  #print(q.payload)
  cat(green("DONE "), "\n")
  toc(); 
  return(q.payload)
  
}
