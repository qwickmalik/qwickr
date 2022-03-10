#' @title Generate PK parameters for each subject
#' 
#' @aliases qwickr.pkparams
#' 
#' @description Generate PK parameters for each subject 
#' 
#' @usage qwickr.pkparams(db, suffix="")
#' 
#' @param db Data frame. See example for format
#' @param suffix string/text to be included in the list item name 
#' 
#' @details Generates PK parameters for each subject, analyte and study group. 
#' @return Returns a list containing data frames of PK parameters for each analyte and study group, and one data frame containing data for all participants.
#' 
#' @author Abdul Malik Sulley <asulley@uwo.ca> Feb 12, 2021
#' 
#' @examples
#' db <- pkdata
#' qwickr.pkparams(db)
#' 
#' @import crayon
#' @importFrom stringr "%>%"
#' @importFrom rlang .data
#' @export
#'

qwickr.pkparams <- function(db, suffix=""){
  tictoc::tic()
  q.payload <- q.newPayload()
  
  
  db2 <- q.netconcs(db)
  
  myinterval <- unique(db2$TIME)
  
  min_interval <- min(myinterval, na.rm=T)
  max_interval <- max(myinterval, na.rm=T)
  # cat("Min Interval: ", min_interval)
  # return(min_interval)
  pk.output <- db2 %>% 
    dplyr::group_by(.data$SUBJECTNUM, .data$ANALYTE, .data$ANALYTEN, .data$GROUPING, .data$TRT, .data$SEQ, .data$PERIOD) %>% 
    dplyr::summarise(
      AUC = PKNCA::pk.calc.aucint.all(.data$CONC, .data$TIME, start = min_interval, end = max_interval, method = "linear"),
      AUC.1 = DescTools::AUC(.data$TIME, .data$CONC, method = "linear", na.rm = FALSE), #same results as above
      LOGAUC = log(.data$AUC),
      NETAUC.1 = DescTools::AUC(.data$TIME, .data$VALUEDIFF, method = "linear",  na.rm = FALSE),
      NETAUC = PKNCA::pk.calc.aucint.all(.data$VALUEDIFF, .data$TIME, interval = c(min_interval, max_interval), method = "linear"), 
      #LOGNETAUC = log(NETAUC),
      LOGNETAUC = NA,
      AUMC = PKNCA::pk.calc.aumc (.data$CONC, .data$TIME),
      CMAX = PKNCA::pk.calc.cmax(.data$CONC, check = TRUE),
      LOGCMAX = log(.data$CMAX),
      TMAX = PKNCA::pk.calc.tmax(.data$CONC, .data$TIME, check = TRUE),
      MRT = PKNCA::pk.calc.mrt(.data$AUC, .data$AUMC),                
      KE = PKNCA::pk.calc.kel(.data$MRT),   # ke: terminal disposition rate constant
      HALFLIFE = log(2)/.data$KE, # halflife: terminal half life
      AUCINF = PKNCA::pk.calc.auc.inf(.data$CONC, .data$TIME, lambda.z = .data$KE),
      LOGAUCINF = log(.data$AUCINF),
      AUCPERCENT = (.data$AUC / .data$AUCINF) *100,
      KA = NA
    )
  metabolites <- unique(db2$ANALYTEN)
  analytes <- unique(db2$ANALYTE)
  cat("Analytes", metabolites, "\n")
  subjects <- unique(db2$SUBJECTNUM)
  cat("Subjects: ", subjects, "\n")
  # return(pk.output)
  
  for (m in metabolites) {
    # cat("Analyte: ", m, "\n")
    analyte <- analytes[as.numeric(m)]
    w2 <- db2[ !is.na(db2$CONC) & db2$ANALYTEN == m, ]
    for (s in subjects) {
      # cat("Subject ID: ", s, "\n")
      db <- w2[ w2$SUBJECTNUM == s, ]
      db$LCONC <- log(db$CONC)
      db$LCONC[is.infinite(db$LCONC)] <- NA
      
      ke <- pk.output$KE[pk.output$SUBJECTNUM == s & pk.output$ANALYTEN == m]
      Tmax <- pk.output$TMAX[pk.output$SUBJECTNUM == s & pk.output$ANALYTEN == m]
      # cat("Ke: ", ke, "\n")
      
      ##ka
      ka.coeff <- ke * (-1)
      # cat("Ka.coeff: ", ka.coeff, "\n")
      
      db3 <- db[db$TIME < Tmax,]
      db3$CONC_EXTRAP <- ka.coeff * db3$TIME
      db3$CONC_EXTRAP2 <-  exp(db3$CONC_EXTRAP)
      db3$CONC_DIFF <-  db3$CONC_EXTRAP2 - db3$CONC
      db3$CONC_DIFF2 <-  db3$CONC_EXTRAP - db3$CONC
      # plot(x=db3$TIME, y=db3$CONC_DIFF, type="o", col="blue"); 
      # plot(x=db3$TIME, y=db3$CONC_DIFF2, type="o", col="blue"); 
      
      ka.mod <- lm(CONC_DIFF ~ TIME, data = db3)
      ka.mod.summary <- summary(ka.mod)
      ka.mod.summary.db <- as.data.frame(ka.mod.summary$coefficients)
      ka.mod.summary.db
      ka1 <- ka.mod.summary.db["TIME", "Estimate"]
      ka <- 2.303 * ka1 * (-1)
      ka
      
      pk.output$KA[pk.output$SUBJECTNUM == s & pk.output$ANALYTEN == m] <- ka
    }
  }
  
  
  
  metabolites <- unique(pk.output$ANALYTEN)
  trt <- unique(pk.output$TRT)
  analytes <- unique(pk.output$ANALYTE)
  metabolites
  pk.out <- pk.output[nrow(0),]
  
  for (m in metabolites) {
    cat("Metabolite: ", m, "\n")
    v <- pk.output[pk.output$ANALYTEN == m, ]
    # print(head(v))
    netauc.min <- min(v$NETAUC, na.rm = T)
    # print(netauc.min)
    if(netauc.min < 0){
      v$LOGNETAUC <- log(v$NETAUC - netauc.min + 1)
    } else {
      v$LOGNETAUC <- log(v$NETAUC)
    }
    
    pk.out <- rbind.data.frame(pk.out, v)
  }
  
  q.payload[[paste("PK_params_combined", suffix, sep = "_")]] <- pk.out
  
  for (m in metabolites) {
    analyte <- analytes[as.numeric(m)]
    # print(analyte)
    for (t in trt) {
      # print(t)
      pk.out1 <- pk.out[pk.out$ANALYTEN == m & pk.out$TRT == t, c( "ANALYTE", "ANALYTEN","GROUPING", "TRT","SUBJECTNUM", "SEQ", "PERIOD", 
                                                                     "AUC", "LOGAUC", "AUCINF", "LOGAUCINF", "NETAUC", "LOGNETAUC",
                                                                     "CMAX", "LOGCMAX", "TMAX","KE", "HALFLIFE", "KA") ]
      # write.csv(pk.out1, file = paste0(output_path_main, popn, "/", analyte, "/PK_params_", suffix, "_", analyte, "_Group_", t, ".csv"))
      q.payload[[paste("PK_parameters", suffix, analyte, t, sep = "_")]] <- pk.out1
    }
    
  }
  
  
  
  
  cat(green("\u221a PK parameters generated"), "\n")
  tictoc::toc(); 
  return(q.payload)
  
}

