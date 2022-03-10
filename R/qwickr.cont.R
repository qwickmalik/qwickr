#' @title Summarize continuous data and test hypotheses
#' @aliases qwickr.cont
#' @description Analyze continuous data and test hypotheses
#' @usage qwickr.cont(db, design="parallel", outcomevar="", idvar="", groupvar="", 
#' timevar="", visitnumbers=c(), baselinevisit="", speccomp1=NA, speccomp2=NA, 
#' lloq=NA, mimp="cart", locf=FALSE,  noimp=TRUE, runpairwise=FALSE, adj=FALSE,
#' within.group="t-test", covs="FFFFTF", assume.normal.dist=FALSE, 
#' useranks=FALSE, useglm=FALSE, usegee=FALSE, glmgeefamily=NULL,
#' exportfile=c(".doc"), exportpath="", dbexport="", filesuffix="")
#' @param db Data frame
#' @param design specify the study design. Options: c("parallel", "crossover"). 
#' @param idvar Name of the unique subject/particpant ID variable \code{(string)}
#' @param outcomevar Name of outcome variable \code{(string)}
#' @param groupvar Name of grouping variable \code{(string)}
#' @param timevar Name of the time variable \code{(string)}
#' @param visitnumbers A vector of visit numbers to be included in analysis, excluding reference visit (baseline/screening)
#' @param baselinevisit Reference visit number (baseline/screening)
#' @param speccomp1 Special visit numbers to compare in addition to comparison to baseline. must be a subset of visitnumbers. speccomp1 is the baseline to which speccomp2 is compared. e.g. to compare visits 3 and 4, speccomp1=3, speccomp2=4
#' @param speccomp2 Special visit numbers to compare in addition to comparison to baseline. must be a subset of visitnumbers. speccomp1 is the baseline to which speccomp2 is compared. e.g. to compare visits 3 and 4, speccomp1=3, speccomp2=4
#' @param lloq value to impute everywhere there's a value less than the lower limit of quantification, denoted by -33
#' @param mimp conduct multiple imputation? If yes, specify imputation method for multiple imputations (default: Classification and Regression Trees (cart)). See help docs for MICE package for more options. If multiple imputation not desired, use ""
#' @param locf conduct LOCF imputation? (T/F)
#' @param noimp run analysis for non-imputed dataset? (default=T)
#' @param runpairwise conduct between-group pairwise comparisons? (T/F)
#' @param adj adjust alpha for between-group pairwise comparisons? (T/F)
#' @param within.group calculate change in outcome from ‘baselinevisit’ to all other visits and conduct within-group paired comparisons using the mixed model (option: "model"), paired t-tests/Wilcoxon Signed Rank Test (option: "t-test"), both (option: "both"). If change in outcome from ‘baseline’ is not desired, no within-group comparisons will be done (option: "none")
#' @param covs Covariance structures to test for mixed-models.Options: Compound Symmetry (T/F), Heterogeneous CS (T/F), #Autoregressive (T/F), Heterogeneous Autoregressive (T/F), no covariance structure (T/F), no covariance structure using lme4::lmer() (T/F). Default: \code{"FFFFTF"}
#' @param assume.normal.dist assume that the data is normally distributed? (T/F)
#' @param useranks in the case of intractable non-normality & \code{assume.normal.dist=F}, use ranked ANOVA instead of Wilcoxon/Kruskal Wallis (T/F)
#' @param useglm analyze raw data using Poisson distribution OR perform logistic regression? (T/F). Specify appropriate \code{glmgeefamily}
#' @param usegee analyze raw data using General Estimating Equations? (T/F). Specify appropriate \code{glmgeefamily}
#' @param glmgeefamily specify which family to use e.g. for logistic regression, use \code{"binomial"} and set \code{useglm=T}; for Poisson, use \code{"poisson"}
#' @param exportfile Export the output to file? Options: \code{c(".csv", ".doc")}. See \code{q.write.to.word, stats::write.csv }
#' @param exportpath Path relative to the working directory where exported files will be saved e.g. "OUTPUT" Do not begin or end with a backslash. If left empty, file will be exported to the working directory.
#' @param dbexport specify which dataset to be exported. Options: c("noimp", "locf", "mimp", "")
#' @param filesuffix filesuffix to be included in the file name for the exported output file
#' 
#' @details Analyze continuous data and test hypotheses
#' @return Returns a list containing data frame of means, standard deviations, medians, minimum-maximum ranges for each study arm and an associated p-value for each study time point in a parallel or crossover, repeated measures or non-repeated measures design.
#' 
#' @author Abdul Malik Sulley
#' 
#' @examples 
#' q.data <- rmdata
#' qwickr.cont(db=q.data, design="parallel", outcomevar="BIOMARKER", idvar="SUBJECTNUM",
#' groupvar="GROUPING", timevar="VISITNUMBER", baselinevisit="1", visitnumbers=c(2:4), 
#' mimp="", locf=TRUE,  noimp=TRUE,
#' runpairwise=FALSE, adj=FALSE, within.group="t-test", covs="FFFFTF", 
#' assume.normal.dist=FALSE, 
#' useranks=TRUE, useglm=FALSE, usegee=FALSE, glmgeefamily=NULL, 
#' exportfile=c(".doc"), exportpath="", dbexport="", filesuffix="mysuffix")
#' @import tictoc
#' @import crayon
#' @import emmeans
#' @import stringr
#' @import utils
#' @import stats
#' @import car
#' @export
#'
#'

qwickr.cont <- function(db, design="parallel", outcomevar="", idvar="", groupvar="", timevar="",
                    visitnumbers=c(), baselinevisit="", speccomp1=NA, speccomp2=NA, lloq=NA, 
                    mimp="cart", locf=FALSE,  noimp=TRUE, runpairwise=FALSE, adj=FALSE,
                    within.group="t-test", covs="FFFFTF", assume.normal.dist=FALSE, 
                    useranks=FALSE, useglm=FALSE, usegee=FALSE, glmgeefamily=NULL,
                    exportfile=c(".doc"), exportpath="", dbexport="", filesuffix=""){

  
  #♣ Housekeeping ♣####
  project_code = "QWICKR"
  popn = "ITT"
  subgroup = ""
  wd <- getwd()
  output_path <- wd
  
  
  #formatting stuff
  nt = "\t"  #new tab
  nl = "\n"  #new line
  nll = "\r"  #new paragraph/carriage return
  
  payload <-  q.newPayload()
  # payload <<- payload.pairwise <<- payload.pchange <<-  list()
  # payload <<- payload.pairwise <<- payload.pchange <<-  q.newPayload()

  ### Micosoft Word output objects
  # output.doc <- rtf::RTF(paste0(output_path, project_code, "Output.doc"))
  # output.pairwise.doc <- rtf::RTF(paste0(output_path, project_code, "Output.Pairwise.doc"))
  # output.pchange.doc <- rtf::RTF(paste0(output_path, project_code, "Output.PercentChange.doc"))
  
  
  ## Export path for main output
  if(exportpath != ""){ 
    exppath <- paste0(getwd(), "/", exportpath, "/") 
  } else { 
    exppath <- paste0(getwd(), "/")
  }
  

  
  
  
  
  #+--------------------------+#
  #♣ HELPERS ♣####
  #+--------------------------+#
  #show plots in 2x2 grid
  graphics::par(mfrow=c(1,1))

  
  ##formatting  to required decimal places
  n_decimals <- function(x, k) {
    if(!is.null(x)){
      trimws(format(round(x, k), nsmall=k))
    }
  }
  
  
  substring_right <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  ##Recode missing values
  Recode_NA = function(db){
    db[db == (-99) | db == (-77) | db == (-55) | db == (-33) | db == "NaN"] <- NA
    #db[db == "-99" | db == "-77" | db == "-55"] <- NA
    return(db)
  }
  
  
  RecodeLLOQ_NA = function(db, lloq){
    db[db == (-33) ] <- lloq
    return(db)
  }
  
  RecodeLLOQ = function(db, oldvalue, newvalue){
    db[db == oldvalue ] <- newvalue
    return(db)
  }
  
  
  #Multiple imputation
  q.mimp <- function(db, mygroups, imp_method){
    t3 <- db
    #for (i in mygroups) {
    #t4 <- t3[t3$GROUPING == i,]
    t4 <- t3
    tempData <- mice::mice(t4, m=5, maxit=1, meth=c(imp_method), seed = 123)
    #print(tempData$loggedEvents)
    t5 <- mice::complete(tempData, 3)
    if(!exists("t.imp.mimp")){
      t.imp.mimp <- t5
      t.imp.mimp[is.na(t.imp.mimp$SUBJECTNUM),] <- NULL
    } else {
      t.imp.mimp <- rbind.data.frame(t.imp.mimp, t5)
    }
    return(t.imp.mimp)
    #}
  }
  
  ##Convert variables to factors
  AsFactor <- function(dataset){
    
    dataset$SUBJECTNUM <- as.factor(dataset$SUBJECTNUM)
    
    if("VISITNUMBER" %in% names(dataset)){ dataset$VISITNUMBER <- as.factor(dataset$VISITNUMBER) }
    if("GROUPING" %in% names(dataset)){ dataset$GROUPING <- as.factor(dataset$GROUPING) }
    if("SEQ" %in% names(dataset)){ dataset$SEQ <- factor(dataset$SEQ) }
    if("SITE" %in% names(dataset)){ dataset$SITE <- factor(dataset$SITE) }
    
    return(dataset)
  }
  
  MakeFactor = function( db, fields=NULL ) {
    
    if(!is.null(fields)){
      Vars=names(fields)
    } else {
      Vars=names(db)
    }
    
    for( Var in Vars ) {
      db[,Var] = as.factor( db[,Var] )
    }
    db 
  }
  
  MakeNumeric <- function( db, fields=NULL ) {
    
    if(!is.null(fields)){
      Vars=names(db[fields])
      print(Vars)
    } else {
      Vars=names(db)
    }
    
    for( Var in Vars ) {
      db[Var] = as.numeric( unlist(db[Var]) )
    }
    return(db)
  }
  
  
  MakeCharacter <- function( db, fields=NULL ) {
    
    if(!is.null(fields)){
      Vars=names(db[fields])
      print(Vars)
    } else {
      Vars=names(db)
    }
    
    for( Var in Vars ) {
      db[Var] = as.character( unlist(db[Var]) )
    }
    return(db)
  }
 
  ## Test for normality
  normality <- function(x){
    cat(paste0("Plain:   ",shapiro.test(x)$p.value,"\n"))
    cat(paste0("Log:     ",shapiro.test(log(x+1))$p.value,"\n"))
    cat(paste0("SQRT:    ",shapiro.test(sqrt(x))$p.value,"\n"))
    cat(paste0("Squared: ",shapiro.test((x^2))$p.value,"\n"))
    
    #plots
    a <- qqnorm(x)
    b <- qqline(x) 
    c <- graphics::hist(x)
    
    
  }
  
  normality2 <- function(x, field){
    cat(paste0("Plain:   ",shapiro.test(field)$p.value,"\n"))
    #cat(paste0("Log:     ",shapiro.test(log(field+1))$p.value,"\n"))
    #cat(paste0("SQRT:    ",shapiro.test(sqrt(field))$p.value,"\n"))
    #cat(paste0("Squared: ",shapiro.test((field^2))$p.value,"\n"))
    
    #plots
    #a <- qqnorm(field)
    #b <- qqline(field) 
    #c <- hist(x)
    
    ggplot2::ggplot(x, ggplot2::aes(x = field)) + 
      ggplot2::geom_histogram(binwidth = 1/5) + 
      ggplot2::theme(text = ggplot2::element_text(size = 20))
    
    #ggarrange(c,
    #          labels = c("A", "B", "C"),
    #          ncol = 1, nrow = 1)
  }
  
  ## Test ANOVA/ANCOVA assumptions based on linear model
  assumptions <- function(x){
    cat("Shapiro Test: ",shapiro.test(resid(x))$p.value, nl) #test for nomality
    
    graphics::hist( resid(x), breaks=25 )
    
    
    #test for normality of residuals
    qqnorm(resid(x))
    qqline(resid(x)) 
    
    # test for independence of sampling
    graphics::plot(fitted(x),resid(x))
    graphics::abline(h=0, lty=2)
    #lines(smooth.spline(fitted(x),residuals(x)))
    
    cat("Levene Test: ",leveneTest(x)$`Pr(>F)`[1], nl, nl) #test for homogeneity of variances (centered around the median)
  }
  
  assumptions.mixed <- function(x, output_path, analyte, subject){
    #cat("Shapiro Test: ",shapiro.test(resid(x))$p.value, nl) #test for nomality
    
    #hist( resid(x), breaks=25 )
    
    #test for normality of residuals
    qqnorm(resid(x))
    qqline(resid(x)) 
    
    ## test for independence of sampling
    graphics::plot(fitted(x),resid(x))
    graphics::abline(h=0, lty=2)
    #lines(smooth.spline(fitted(x),residuals(x)))
    
  }
  
  ##Descriptive Summaries
  f <- function(x) {
    c(N = round(length(na.omit(x)), 0), MED = round(arsenal::medianrange(x, na.rm = TRUE), 2), MN = round(mean(x, na.rm = TRUE), 2), SD = round(sd(x, na.rm = TRUE), 2))
  }
  
  f2 <- function(x) {
    c(N = round(length(na.omit(x)), 0), MN = round(mean(x, na.rm = TRUE), 2), SD = round(sd(x), 2))
  }
  
  f3 <- function(x) {
    c(N = round(length(na.omit(x)), 0), MN = round(mean(x, na.rm = TRUE), 2))
  }
  
  mymean <- function(x) {
    round(mean(x, na.rm = TRUE), 3)
  }
  
  geomean <- function(x) {
    c(N = round(length(na.omit(x)), 0), MN = round(mean(x, na.rm = TRUE), 2), GeoMN = round(exp(mean(log(x), na.rm = TRUE)), 2) )
  }
  
  
  GenDataFramesTRT <- function(db,  baselinevisit, visitnumbers, speccomp1=NA, speccomp2=NA){
    t <- db
    if("SEQ" %in% colnames(t)) t$SEQ <- t$SEQ else t$SEQ <- NA
    if("PERIOD" %in% colnames(t)) t$PERIOD <- t$PERIOD else t$PERIOD <- NA
    if("TRT" %in% colnames(t)) t$TRT <- t$TRT else t$TRT <- NA
    if("SITE" %in% colnames(t)) t$SITE <- t$SITE else t$SITE <- NA
    if("GENDER" %in% colnames(t)) t$GENDER <- t$GENDER else t$GENDER <- NA
    if("AGE" %in% colnames(t)) t$AGE <- t$AGE else t$AGE <- NA
    baseline <- t[t$VISITNUMBER == baselinevisit, c("SUBJECTNUM", "PERIOD", "OUTCOME") ] #Baseline
    colnames(baseline)[colnames(baseline) == "OUTCOME"] <- "BASELINE"
    t.df <- merge(x = t, y = baseline, by = c("SUBJECTNUM", "PERIOD"), all.x = TRUE)
    
    
    #prepare data for change from baseline
    #visno <- c(baselinevisit, visitnumbers)
    visno <- NA
    if(isTRUE(baselinevisit == visitnumbers)){
      visno <- baselinevisit
    } else {
      visno <- unique(c( baselinevisit, visitnumbers ))
      visno <- visno[!is.na(visno)]
    }
    for(i in visno){
      visitz <- t.df[t.df$VISITNUMBER == i, c("SUBJECTNUM", "PERIOD", "OUTCOME")] 
      t.df <- merge(x = t.df, y = visitz, by = c("SUBJECTNUM", "PERIOD"), all.x = TRUE)
      myvisit = as.character(paste0("VISIT",i))
      colnames(t.df)[colnames(t.df) == "OUTCOME.y"] <- myvisit
      colnames(t.df)[colnames(t.df) == "OUTCOME.x"] <- "OUTCOME"
      mychange = as.character(paste0("CHANGE",i))
      t.df[,c(mychange)] <- t.df[,c(myvisit)] - t.df$BASELINE
      #head(t.df)
    }
    #prepare dataset for special change
    if(!is.na(speccomp1) & !is.na(speccomp2)){
      visit1 <- t.df[t.df$VISITNUMBER == speccomp1, c("SUBJECTNUM", "PERIOD", "OUTCOME")] 
      visit2 <- t.df[t.df$VISITNUMBER == speccomp2, c("SUBJECTNUM", "PERIOD", "OUTCOME")] 
      t.df <- merge(x = t.df, y = visit1, by = c("SUBJECTNUM", "PERIOD"), all.x = TRUE)
      myvisit1 = as.character(paste0("VISITS",speccomp1))
      colnames(t.df)[colnames(t.df) == "OUTCOME.y"] <- myvisit1
      t.df <- merge(x = t.df, y = visit2, by = c("SUBJECTNUM", "PERIOD"), all.x = TRUE)
      myvisit2 = as.character(paste0("VISITS",speccomp2))
      colnames(t.df)[colnames(t.df) == "OUTCOME"] <- myvisit2
      t.df$OUTCOME.y <- NULL; t.df$OUTCOME <- NULL
      colnames(t.df)[colnames(t.df) == "OUTCOME.x"] <- "OUTCOME"
      mychange = "CHANGE_SP"
      t.df[, "BASELINE"] <- ifelse(t.df$VISITNUMBER %in% c(speccomp1, speccomp2), t.df[,c(myvisit1)], t.df[, "BASELINE"])
      t.df[,c(mychange)] <- t.df[,c(myvisit2)] - t.df[,c(myvisit1)]
      #head(t.df)
    }
    
    #return(t.df)
    #u <- data.frame(SUBJECTNUM=factor(), GROUPING=factor(), VISITNUMBER=factor(), BASELINE=numeric(), OUTCOME=numeric(), CHANGE=numeric(), stringsAsFactors=FALSE)
    u <- NULL
    
    for(i in visno){
      mychange = as.character(paste0("CHANGE",i))
      #t3 <- t.df[which(t.df$VISITNUMBER == i), c("SUBJECTNUM", "GROUPING", "VISITNUMBER", "BASELINE", "OUTCOME", mychange)] 
      t3 <- t.df[which(t.df$VISITNUMBER == i), c("SUBJECTNUM","AGE", "GENDER", "GROUPING", "VISITNUMBER", "BASELINE", "OUTCOME", "PERIOD", "SEQ", "TRT", "SITE", mychange)]
      if(!is.na(speccomp1) & !is.na(speccomp2)){t3$CHGNUM <- 1001}
      colnames(t3)[colnames(t3) == mychange] <- "CHANGE"
      u <- rbind.data.frame(u, t3)
    }
    
    if(!is.null(t.df$CHANGE_SP)){
      if(!speccomp1 %in% visno){
        t3 <- t.df[which(t.df$VISITNUMBER %in% c(speccomp1, speccomp2)), c("SUBJECTNUM","AGE", "GENDER", "GROUPING", "VISITNUMBER", "BASELINE", "OUTCOME", "PERIOD", "SEQ", "TRT", "SITE")]
        t3$CHANGE <- NA
        t3$CHGNUM <- 1001
        u <- rbind.data.frame(u, t3)
      }
      
      t4 <- t.df[which(t.df$VISITNUMBER == speccomp1), c("SUBJECTNUM", "AGE", "GENDER", "GROUPING", "VISITNUMBER",  "BASELINE", "OUTCOME", 
                                                         "PERIOD", "SEQ", "TRT", "SITE", "CHANGE_SP")]; 
      #t4 <- t.df[which(t.df$VISITNUMBER == speccomp1), ]; 
      t4$VISITNUMBER <- paste0(speccomp1, speccomp2)
      t4$CHGNUM <- paste0(speccomp1, speccomp2)
      colnames(t4)[colnames(t4) == "CHANGE_SP"] <- "CHANGE"
      u <- rbind.data.frame(u, t4)
    }
    
    u$CHANGE <- ifelse(u$VISITNUMBER == baselinevisit, NA, u$CHANGE) 
    # u$PERCENT <- (u$CHANGE2/u$BASELINE)*100
    u$PERCENT <- with(u, (ifelse(OUTCOME<0 & BASELINE<0 & OUTCOME>BASELINE, ((OUTCOME-BASELINE)/BASELINE)*-1,
                                 ifelse(OUTCOME<0 & BASELINE<0 & OUTCOME>BASELINE, ((OUTCOME-BASELINE)/BASELINE),
                                        ifelse(OUTCOME<0 & BASELINE<0, ((OUTCOME-BASELINE)/BASELINE)*-1,
                                               ifelse(OUTCOME>0 & BASELINE<0, ((OUTCOME-BASELINE)/BASELINE)*-1,
                                                      ifelse(BASELINE<OUTCOME, ((OUTCOME-BASELINE)/BASELINE),
                                                             ifelse(BASELINE>OUTCOME,((OUTCOME-BASELINE)/BASELINE), abs(((OUTCOME-BASELINE)/BASELINE))))))))*100))
    
    
    
    
    
    
    u <- MakeFactor(u, field=c("SUBJECTNUM", "VISITNUMBER", "GROUPING", "PERIOD", "TRT", "SEQ", "SITE", "PERIOD", "GENDER"))
    #u$SUBJECTNUM <- factor(u$SUBJECTNUM)
    #u$VISITNUMBER <- factor(u$VISITNUMBER)
    #u$GROUPING <- factor(u$GROUPING)
    #u$PERIOD <- factor(u$PERIOD)
    #u$TRT <- factor(u$TRT)
    #u$SEQ <- factor(u$SEQ)
    #u$SITE <- factor(u$SITE)
    ##u$AGEGROUP <- factor(u$AGEGROUP)
    #u$GENDER <- factor(u$GENDER)
    
    #print(summary(u))
    return(u)
  }
  
  
  RenameHeaders <- function(x, demographics=F){
    #x = data frame
    #\u00B6
    for (t in names(x)) {
      if(t == "#") { 
        names(x)[names(x) == t] <- paste0("Study Timepoint") 
        
        #} else if(isTRUE(grepl(pattern="Group", x=t))) { 
      } else if(substring(t, 1, 5) %in% c("Group", "Treat", "Seque", "Inter", "Produ", "Formu") ) { 
        if(isTRUE(demographics)){
          names(x)[names(x) == t] <- paste0(t, "\n","Mean ", "\u00B1", " SD (n)", "\n", "Median (Min - Max)") 
        } else {
          names(x)[names(x) == t] <- paste0(t, "\nMean ", "\u00B1", " SD (n) \nMedian (Min - Max) \nWithin Group P-value") 
        }
        
      } else if(t == "p.val" | t == "p-value") { 
        names(x)[names(x) == t] <- paste0("Between", "\n", "Groups", "\n", "P-value")
        
      }
    }
    
    return(x)
  }

  
  
  ####1a. Main Analysis Function ####
  AnalyzeCombined <- function( db, design, outcomevar, idvar, groupvar, 
                               timevar, visitnumbers, baselinevisit, speccomp1, speccomp2,
                               lloq, mimp, locf,  noimp, demographics, runpairwise, adj,
                               within.group, covs, assume.normal.dist, useranks, useglm, usegee, 
                               glmgeefamily, dbexport, filesuffix ){ # repeated=NA, 
    
    
    
    db["SUBJECTNUM"] <- db[idvar]
    db["GROUPING"] <- db[groupvar]
    db["VISITNUMBER"] <- db[timevar]
    
    o = order( db$SUBJECTNUM, db$VISITNUMBER ); Db = db[o,]
    
    #impute values with lowest limit of quantification results (-33)
    if(!is.na(lloq)){
      cat("LLOQ: ", lloq, nl)
      Db <- RecodeLLOQ_NA(Db, lloq)
    }
    
    #prepare data for descriptives
    t <- Db
    #ggroups <- t$GROUPING
    #ggroups <- as.numeric(unique(ggroups))
    #grps <- length(ggroups)
    #ggroups <- sort(ggroups)
    #grp.names <- ggroups 
    #group.names <- grp.names[1:grps]
    #print(group.names)
    #safety.vars <- names(SafetyLabs.t[27:47])
    #if(outcomevar %in% c("COMPLIANCE", "AGE", "SBP", "DPB", "HR", "WEIGHT", "BMI", safety.vars)){
    #  grp.names <- c("ABC", "BCA", "CAB", "D", "E", "F", "G", "H", "I", "J") 
    #} else {
    #  grp.names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J") 
    #}
    
    #group.names <- grp.names[1:grps]
    #output <- setNames(data.frame(matrix(ncol = grps+1, nrow = 0)), c(group.names, "p.val") )
    
    ggroups <- t$GROUPING
    ggroups <- na.omit(unique(ggroups))
    ggroups <- sort(ggroups)
    grp.names <- ggroups 
    grps <- length(ggroups)
    group.names <- as.vector(grp.names[1:grps])
    cat(blue("Study Arms: " %+% as.character(toString(group.names))), nl)
    
    
    output <- setNames(data.frame(matrix(ncol = grps+2, nrow = 0)), 
                                          c("#", group.names, "p.val") )
    #View(output)
    output.pairwise <- list()
    output.pairwise.adj <- list()
    
    
    ##IMPUTATION
    #Recode missing to NA 
    t.noimp <- Recode_NA(t)
    missingpresent <- t.noimp[is.na(t.noimp[outcomevar]),]
    t.imp.locf <- NA
    t.imp.mimp <- NA
    
    if(length(missingpresent$SUBJECTNUM) > 0){
      cat(red("Missing data present"), nl)
      
      ##LOCF imputation
      t1 <- Recode_NA(t)
      if(isTRUE(locf)){
        ##t2 <- na.locf(t1)
        #t2 <- t1 %>% 
        #  #slice_rows(c("SUBJECTNUM", "GROUPING")) %>% 
        #  slice_rows(c("SUBJECTNUM")) %>% 
        #  by_slice(function(x) { 
        #    na.locf(na.locf(x, na.rm = F), fromLast=T, na.rm = F) },
        #    .collate = "rows")  #locf both directions
        #t.imp.locf <- t2
        
        t.imp.locf <- q.locf(t1, sliceby = c("SUBJECTNUM"))
      }
      
      ##Multiple imputation only
      t3 <- Recode_NA(t)
      #separate dataset according to groups before imputation
      if(mimp != ""){
        #for (i in ggroups) {
        #  t4 <- t3[t3$GROUPING == i,]
        #  tempData <- mice(t4, m=5, maxit=1, meth=c(imp_method), seed = 1)
        #  #print(tempData$loggedEvents)
        #  t5 <- complete(tempData, 3)
        #  if(exists("t.imp.mimp")){
        #    t.imp.mimp <- t5
        #    t.imp.mimp[is.na(t.imp.mimp$SUBJECTNUM),] <- NULL
        #  } else {
        #    t.imp.mimp <- rbind.data.frame(t.imp.mimp, t5)
        #  }
        
        #}
        t.imp.mimp <- q.mimp(t1, mygroups = ggroups, imp_method = mimp)
      }
      
      
    } else {
      t.imp.locf <- t.imp.mimp <- t.noimp
    }
    
    
    if(isTRUE(noimp)){
      t.noimp["OUTCOME"] <- t.noimp[outcomevar]
      c.noimp <- GenDataFramesTRT(t.noimp, baselinevisit, visitnumbers, speccomp1, speccomp2)
      # print(c.noimp)
    }
    if(isTRUE(locf)){
      t.imp.locf["OUTCOME"] <- t.imp.locf[outcomevar]
      c.imp.locf <- GenDataFramesTRT( t.imp.locf, baselinevisit, visitnumbers, speccomp1, speccomp2 )
      
    }
    if(mimp != ""){
      t.imp.mimp["OUTCOME"] <- t.imp.mimp[outcomevar]
      c.imp.mimp <- GenDataFramesTRT( t.imp.mimp, baselinevisit, visitnumbers, speccomp1, speccomp2 )
      
    }
    
    #print("## Final Data Frame - No imputations ##")
    #print(summary(c.noimp))
    #return(c.noimp)
    #print("## Final Data Frame - locf imputations ##")
    #print(summary(c.imp.locf))
    #return(c.imp.locf)
    #print("## Final Data Frame - Multiple imputations only ##")
    #print(summary(c.imp.multi))
    #return(c.imp.multi)
    #cat(yellow("Repeated1: " %+% as.character(toString(repeated))), nl)
    
    
    repeated <- ifelse(length(visitnumbers) == 1 & baselinevisit == visitnumbers, F,
                       ifelse(length(visitnumbers) > 1 | baselinevisit != visitnumbers, T, errorCondition("Please set repeated=T/F")))
    repeated <- unique(repeated)
    if(length(repeated) > 1){errorCondition("Please set repeated=T/F")}
      
    
    
    
    # if(is.na(repeated)){
    #   repeated <- ifelse(baselinevisit == visitnumbers, F, T)
    # } else {
    #   repeated <- repeated
    # }
    if(!isTRUE(demographics)){
      cat(yellow("Repeated: " %+% as.character(toString(repeated))), nl)
    }

    ##Pass data frames to respective functions for summaries
    RunSummariesCombined <- function(c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, 
                                     demographics, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, 
                                     group.names, output, output.pairwise, output.pairwise.adj, design ){
      if(isTRUE(demographics)){
        if(isTRUE(useglm)){
          result <- SumsGLMCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, filesuffix, demographics, runpairwise, adj, assume.normal.dist, 
                           useranks, ggroups, grps, group.names, output, output.pairwise, glmgeefamily=glmgeefamily, design )
          
        } 
        else if(isTRUE(usegee)){
          result <- SumsGEECombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, filesuffix, demographics, runpairwise, adj, assume.normal.dist,
                           useranks, ggroups, grps, group.names, output, output.pairwise, glmgeefamily=glmgeefamily, design )
          
        } 
        else {
          result <- SumsCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, demographics, runpairwise, adj, assume.normal.dist, 
                        useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
          output[NULL,]
          
        }
        
      } else if(isTRUE(repeated)){
        if(isTRUE(useglm)){
          output <- SumsGLMCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, filesuffix, demographics, runpairwise, adj, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, glmgeefamily=glmgeefamily, design )
          result <- ChangeSumsRepeatedCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
          output[NULL,]
          
        } else {
          output <- SumsCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, demographics, runpairwise, adj, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
          result <- ChangeSumsRepeatedCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
          output[NULL,]
          
        }
        
      } else {
        if(isTRUE(useglm)){
          output <- SumsGLMCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, filesuffix, demographics, runpairwise, adj, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, glmgeefamily=glmgeefamily, design )
          result <- ChangeSumsCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
          output[NULL,]
          
        } else {
          output <- SumsCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, demographics, runpairwise, adj, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
          result <- ChangeSumsCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
          output[NULL,]
          
        }
      }
      
      return(result)
    }
    
    
    
    
    if(isTRUE(noimp)){
      c.imp <- c.noimp
      result <- RunSummariesCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix=paste0(filesuffix,"_NoImp"), 
                            demographics, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
    }
    if(isTRUE(locf)){
      c.imp <- c.imp.locf
      result <- RunSummariesCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix=paste0(filesuffix,"_LOCF"), 
                            demographics, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
    }
    if(mimp != ""){
      c.imp <- c.imp.mimp
      result <- RunSummariesCombined( c.noimp, c.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix=paste0(filesuffix,"_Mimp"), 
                            demographics, runpairwise, adj, within.group, covs, assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design )
    }
    
    cat(nl, "Analytical Population: ", popn, nl)
    
    ## Export db if requested
    if(dbexport == "noimp"){
      # write.csv(c.noimp, file = paste0(output_path, "/", outcomevar, ".noimp.db.csv"))
      # return(c.noimp)
      result[["noimp_data"]] <- c.noimp
      
    } else if (dbexport == "locf"){
      # write.csv(c.imp.locf, file = paste0(output_path, "/", outcomevar, ".locf.db.csv"))
      # return(c.imp.locf)
      result[["locf_data"]] <- c.imp.locf
      
    } else if(dbexport == "mimp"){
      if(mimp != ""){
        # write.csv(c.imp.mimp, file = paste0(output_path, "/", outcomevar, ".mimp.db.csv"))
        # return(c.imp.mimp)
        result[["mimp_data"]] <- c.imp.mimp
        
      }
    }
    
    return(result)
  }
  
  
  #####1. Summarize Raw Data #####
  SumsCombined <- function( db, db.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, filesuffix, demographics, runpairwise, adj,
                            assume.normal.dist, useranks, ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design ){
    w <- db
    #print(summary(w))
    w <- db.imp #comment this out to generate descriptives using non-imputed dataset 
    w.imp <- db.imp
    ggroups <- w$GROUPING
    ggroups <- unique(ggroups)
    #print(ggroups)
    ggroups <- sort(ggroups)
    
    
    
    #w <- db[ which( db$LBTESTH == testname),]
    viz <- NA
    if(isTRUE(baselinevisit == visitnumbers)){
      viz <- baselinevisit
    } else {
      viz <- unique(c( baselinevisit, visitnumbers, speccomp1, speccomp2 ))
      viz <- viz[!is.na(viz)]
    }
    
    #prepare output object
    #output <- data.frame(Group.A=character(), Group.B=character(), p.val=numeric(),stringsAsFactors=FALSE)
    
    #summarize data by visit
    s=1
    #bm.btwn.p <- NA
    #bm.pairwise <- data.frame(AB=NA, AC=NA, AD=NA, AE=NA, AF=NA, BC=NA, BD=NA, BE=NA, BF=NA, CD=NA, CE=NA, CF=NA, DE=NA, DF=NA, EF=NA)
    #bbm.pairwise <- data.frame()
    
    
    
    for(i in viz){ 
      u <- w[which( w$VISITNUMBER == i ),]
      u.imp <- w.imp[which( w.imp$VISITNUMBER == i ),]
      
      
      #Descriptives ####
      mysumm <- with( u, aggregate(OUTCOME, by=list(GROUP = GROUPING), FUN = f) )
      #cat("# -------------- VISIT ",i," --------------- #", nl)
      cat(green("v ") %+% blue("TIME POINT: " %+% as.character(i)), "  ");
      #print(mysumm)
      #print(mysumm[2,5])
      
      #print(head(u.imp))
      
      if(isTRUE(assume.normal.dist)){
        if(length(ggroups) == 1){
          p.val <- NA
          
        } else if(length(ggroups) >= 2){
          if(design == "parallel"){
            mysumm.m <- with( u.imp, lm(OUTCOME ~ GROUPING, na.action = na.omit ) )
          } else if(design == "crossover"){
            mysumm.m <- with( u.imp, lm(OUTCOME ~ GROUPING + SEQ + PERIOD, na.action = na.omit ) )
          }
          
          
          result <- try(car::Anova(mysumm.m))
          if(!is.null(result$`Pr(>F)`[1])){
            p.val <- n_decimals(result$`Pr(>F)`[1], 3)
          } else {
            p.val <- NA
          }
          
          EM <- emmeans( mysumm.m, ~GROUPING )
          #return(em.within)
          final.model2 <- update( EM, infer = c( TRUE, TRUE ))
          #em.within <- data.frame( final.model2 )
          #return(em.within)
          #EM4 <- joint_tests( final.model2 )
          #em.btwn <- data.frame( EM4 )
          #return(em.btwn)
          ## pairwise comparisons 
          if(isTRUE(runpairwise)){
            #EM5 <- contrast(final.model2, "consec")
            EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
            em.pairwise <- data.frame( EM5 )
            em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
            em.pairwise$timepoint <- i
            # print(em.pairwise)
            if(isTRUE(adj)){
              EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
              em.pairwise.adj <- data.frame( EM6 )
              em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
              em.pairwise.adj$timepoint <- i
            }
          }
        }
        s=s+1
      } 
      else {
        if(length(ggroups) == 1){
          p.val <- NA
          
        } else {
          if(design == "parallel"){
            assum.m <- lm(OUTCOME ~ GROUPING, data = u.imp)
          } else if(design == "crossover"){
            assum.m <- lm(OUTCOME ~ GROUPING + SEQ + PERIOD, data = u.imp)
          }
          
          #assumptions.mixed(assum.m)
          s.test <- try(shapiro.test(u.imp$OUTCOME)$p.value)
          #s.test <- with(u.imp$data, shapiro.test(OUTCOME)$p.value)
          #normality2(u.imp, u.imp$OUTCOME)
          #cat("Sums Shapiro Test 1: ", s.test, nl)
          if(exists("s.test")) cat(silver("  Shapiro Test for raw data: " %+% as.character(s.test)), nl)
          
          #l.test <- try(leveneTest(resid(assum.m))$`Pr(>F)`[1])
          #l.test <- try(leveneTest(assum.m)$`Pr(>F)`[1])
          if(s.test < 0.01){
            
            if(isTRUE(useranks)){
              u.imp$OUTCOME2 <- rank(u.imp$OUTCOME, ties.method="average")
              
              if(design == "parallel"){
                mysumm.m <- lm(OUTCOME2 ~ GROUPING, data = u.imp, na.action = na.omit )
              } else if(design == "crossover"){
                mysumm.m <- lm(OUTCOME2 ~ GROUPING + SEQ + PERIOD, data = u.imp, na.action = na.omit )
              }
              
              result <- try(car::Anova(mysumm.m))
              if(!is.null(result$`Pr(>F)`[1])){
                p.val <- n_decimals(result$`Pr(>F)`[1], 3)
              } else {
                p.val <- NA
              }
              p.val <- paste0(p.val, " (r)")
              
              EM <- emmeans( mysumm.m, ~GROUPING )
              #return(em.within)
              final.model2 <- update( EM, infer = c( TRUE, TRUE ))
              #em.within <- data.frame( final.model2 )
              #return(em.within)
              #EM4 <- joint_tests( final.model2 )
              #em.btwn <- data.frame( EM4 )
              #return(em.btwn)
              ## pairwise comparisons 
              if(isTRUE(runpairwise)){
                #EM5 <- contrast(final.model2, "consec")
                EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
                em.pairwise <- data.frame( EM5 )
                em.pairwise$p.value <- paste0(n_decimals(em.pairwise$p.value, 3), " (r)")
                em.pairwise$timepoint <- i
                if(isTRUE(adj)){
                  EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
                  em.pairwise.adj <- data.frame( EM6 )
                  em.pairwise.adj$p.value <- paste0(n_decimals(em.pairwise.adj$p.value, 3), " (r)")
                  em.pairwise.adj$timepoint <- i
                }
              }
            } else {
              
              u.imp$OUTCOME1 <- log(u.imp$OUTCOME + (1 - min(u.imp$OUTCOME, na.rm = TRUE)))
              #print("U.IMP SUMMARY: ")
              #print(summary(u.imp))
              
              if(design == "parallel"){
                assum.m2 <- lm(OUTCOME1 ~ GROUPING, data = u.imp)
              } else if(design == "crossover"){
                assum.m2 <- lm(OUTCOME1 ~ GROUPING + SEQ + PERIOD, data = u.imp)
              }
              #assumptions.mixed(assum.m2)
              s.test2 <- shapiro.test(u.imp$OUTCOME1)$p.value
              normality2(u.imp, u.imp$OUTCOME1)
              #cat("Sums Shapiro Test 2: ", s.test2, nl)
              if(exists("s.test2")) cat(silver("  Shapiro Test for raw data: " %+% as.character(s.test2)), nl)
              
              if( s.test2 < 0.01 ){
                
                if(length(ggroups) == 1){
                  p.val <- NA
                  
                } else if(length(ggroups) == 2){
                  result <- wilcox.test(OUTCOME ~ GROUPING, data = u.imp, na.action = na.omit)
                  #p.val <- n_decimals(result$p.value, 3)
                  p.val <- paste0(result$p.value, " (w)")
                  
                } else if(length(ggroups) > 2){
                  result <- kruskal.test(OUTCOME ~ GROUPING, data = u.imp, na.action = na.omit)
                  #p.val <- n_decimals(result$p.value, 3)
                  p.val <- paste0(result$p.value, " (k)")
                  
                  if(isTRUE(runpairwise)){
                    #em.pairwise <- data.frame(AB=NA, AC=NA, BC=NA, AD=NA, BD=NA, CD=NA, AE=NA, BE=NA, CE=NA, DE=NA, AF=NA, BF=NA, CF=NA, DF=NA, EF=NA)
                    em.pairwise <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("contrast", "p.value") )
                    posthoc <- PMCMRplus::kwAllPairsNemenyiTest(OUTCOME ~ GROUPING, data = u.imp, p.adjust.method=NULL)
                    print(posthoc)
                    
                    #em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3],
                    #                posthoc$p.value[4,1], posthoc$p.value[4,2], posthoc$p.value[4,3], posthoc$p.value[4,4],
                    #                posthoc$p.value[5,1], posthoc$p.value[5,2], posthoc$p.value[5,3], posthoc$p.value[5,4], posthoc$p.value[5,5]
                    #)
                    
                    #grpcount <- length(ggroups)
                    #em.pair.p <- NULL
                    #if(grpcount == 6){
                    #  em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                  posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3],
                    #                  posthoc$p.value[4,1], posthoc$p.value[4,2], posthoc$p.value[4,3], posthoc$p.value[4,4],
                    #                  posthoc$p.value[5,1], posthoc$p.value[5,2], posthoc$p.value[5,3], posthoc$p.value[5,4], posthoc$p.value[5,5])
                    #} else if(grpcount == 5){
                    #  em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                  posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3],
                    #                  posthoc$p.value[4,1], posthoc$p.value[4,2], posthoc$p.value[4,3], posthoc$p.value[4,4])
                    #} else if(grpcount == 4){
                    #  em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                  posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3])
                    #  
                    #} else if(grpcount == 3){
                    #em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2])
                    
                    #}
                    #print(em.pair.p)
                    #em.pairwise[n,] <- rbind(em.pair.p)
                    #em.pairwise <- data.frame(em.pair.p)
                    #n=n+1
                    
                    #em.btwn <- data.frame( em.btwn.p )
                    #names( em.btwn ) <- "p.value"
                    #cat("#-- Between p-values from Kruskal test", nl)
                    #print(em.btwn.p) 
                    #em.btwn <- data.frame( em.btwn.p )
                    #names( em.btwn ) <- "p.value"
                    posthoc1 <- as.matrix(posthoc$p.value)
                    posthoc2 <- as.data.frame(as.table(posthoc1))
                    posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
                    posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
                    posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
                    em.pairwise <- posthoc3[c("contrast", "p.value")]
                    em.pairwise$timepoint <- i
                    print(em.pairwise)
                    
                    if(isTRUE(adj)){
                      em.pairwise.adj <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("contrast", "p.value") )
                      posthoc <- PMCMRplus::kwAllPairsNemenyiTest(OUTCOME ~ GROUPING, data = u.imp, p.adjust.method="single-step")
                      print(posthoc)
                      posthoc1 <- as.matrix(posthoc$p.value)
                      posthoc2 <- as.data.frame(as.table(posthoc1))
                      posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
                      posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
                      posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
                      em.pairwise.adj <- posthoc3[c("contrast", "p.value")]
                      em.pairwise.adj$timepoint <- i
                      print(em.pairwise.adj)
                    }
                  }
                  
                }
                s=s+1
                
                
              } else {
                if(length(ggroups) == 1){
                  p.val <- NA
                  
                } else if(length(ggroups) >= 2){
                  
                  if(design == "parallel"){
                    mysumm.m <- lm(OUTCOME1 ~ GROUPING, data = u.imp, na.action = na.omit )
                  } else if(design == "crossover"){
                    mysumm.m <- lm(OUTCOME1 ~ GROUPING + SEQ + PERIOD, data = u.imp, na.action = na.omit )
                  }
                  result <- try(car::Anova(mysumm.m))
                  if(!is.null(result$`Pr(>F)`[1])){
                    p.val <- n_decimals(result$`Pr(>F)`[1], 3)
                  } else {
                    p.val <- NA
                  }
                  p.val <- paste0(p.val, " (l)")
                  
                  EM <- emmeans( mysumm.m, ~GROUPING )
                  #return(em.within)
                  final.model2 <- update( EM, infer = c( TRUE, TRUE ))
                  #em.within <- data.frame( final.model2 )
                  #return(em.within)
                  #EM4 <- joint_tests( final.model2 )
                  #em.btwn <- data.frame( EM4 )
                  #return(em.btwn)
                  ## pairwise comparisons 
                  if(isTRUE(runpairwise)){
                    #EM5 <- contrast(final.model2, "consec")
                    EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
                    em.pairwise <- data.frame( EM5 )
                    em.pairwise$p.value <- paste0(n_decimals(em.pairwise$p.value, 3), " (l)")
                    em.pairwise$timepoint <- i
                    if(isTRUE(adj)){
                      EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
                      em.pairwise.adj <- data.frame( EM6 )
                      em.pairwise.adj$p.value <- paste0(n_decimals(em.pairwise.adj$p.value, 3), " (l)")
                      em.pairwise.adj$timepoint <- i
                    }
                  }
                }
                s=s+1
              }
            }
            
          } else {
            if(length(ggroups) == 1){
              p.val <- NA
              
            } else if(length(ggroups) >= 2){
              mysumm.m <- lm(OUTCOME ~ GROUPING, data = u.imp, na.action = na.omit )
              if(design == "parallel"){
                mysumm.m <- lm(OUTCOME ~ GROUPING, data = u.imp, na.action = na.omit )
              } else if(design == "crossover"){
                mysumm.m <- lm(OUTCOME ~ GROUPING + SEQ + PERIOD, data = u.imp, na.action = na.omit )
              }
              result <- try(car::Anova(mysumm.m))
              if(!is.null(result$`Pr(>F)`[1])){
                p.val <- n_decimals(result$`Pr(>F)`[1], 3)
              } else {
                p.val <- NA
              }
              
              
              EM <- emmeans( mysumm.m, ~GROUPING )
              #return(em.within)
              final.model2 <- update( EM, infer = c( TRUE, TRUE ))
              #em.within <- data.frame( final.model2 )
              #return(em.within)
              #EM4 <- joint_tests( final.model2 )
              #em.btwn <- data.frame( EM4 )
              #return(em.btwn)
              ## pairwise comparisons 
              if(isTRUE(runpairwise)){
                #EM5 <- contrast(final.model2, "consec")
                EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
                em.pairwise <- data.frame( EM5 )
                em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
                em.pairwise$timepoint <- i
                if(isTRUE(adj)){
                  EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
                  em.pairwise.adj <- data.frame( EM6 )
                  em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
                  em.pairwise.adj$timepoint <- i
                }
              }
            }
            s=s+1
          }
        }
        
        
      }
      
      ## Output things ####
      #table header
      #header.output.list <- list()
      #header.output.list[["#"]] <- outcomevar
      #for(head_item in 1:grps) {
      #  header.output.list[[group.names[head_item]]] <- ""
      #}
      #header.output.list[["p.val"]] <- ""
      #output[nrow(output) + 1,] = header.output.list
      #output.pairwise[nrow(output.pairwise) + 1,] = header.output.list
      
      
      #format p-value
      the.p.val <- as.character(p.val)
      #cat("the.p.val", the.p.val, nl)
      if(substring_right(the.p.val, 3) %in% c("(l)", "(r)", "(w)", "(k)")){
        filesuffix <- substring_right(the.p.val, 3)
        a <- substr(the.p.val, 1, 6)
        a <- as.numeric(a)
        p.val <- n_decimals(a, 3)
        p.val <- paste0(p.val, " ", filesuffix)
      } else {
        a <- as.numeric(the.p.val)
        p.val <- n_decimals(a, 3)
        p.val <- as.character(p.val)
      }
      
      
      #means & SDs
      ms.output.list <- list()
      #ms.output.list[["#"]] <- paste0("Visit ", i)
      ms.output.list[["#"]] <- i
      for(j in 1:grps) {
        ms.output.list[[group.names[j]]] <- paste0( n_decimals(mysumm$x[j,5], 2), " \u00B1 ", n_decimals(mysumm$x[j,6], 2), " (",mysumm$x[j,1],")" )
      }
      ms.output.list[["p.val"]] <- p.val
      output[nrow(output) + 1,] = ms.output.list
      
      #medians & ranges
      mr.output.list <- list()
      mr.output.list[["#"]] <- " "
      for(m in 1:grps) {
        mr.output.list[[group.names[m]]] <- paste0( n_decimals(mysumm$x[m,2], 2), " (", n_decimals(mysumm$x[m,3], 2), " to ", n_decimals(mysumm$x[m,4], 2), ")" )
      }
      mr.output.list[["p.val"]] <- " "
      output[nrow(output) + 1,] = mr.output.list
      
      
      # if(isTRUE(runpairwise)){
      #   #output.pairwise[nrow(output.pairwise) + 1,] = list(paste0("Visit ", i))
      #   output.pairwise[nrow(output.pairwise) + 1,] = list(paste0(i))
      # 
      #   #print("#===== EM-PAIRWISE ====#")
      #   #print(em.pairwise)
      #   #write.csv(em.pairwise, file=paste0(output_path, "/", outcomevar, "_Visit", i, ".csv"))
      #   #unadjusted
      #   unadj.output.list <- list()
      #   unadj.output.list[["#"]] <- "Pairwise unadj"
      #   for(q in 1:grps) {
      #     unadj.output.list[[group.names[q]]] <- paste0(em.pairwise$p.value[q], " (", em.pairwise$contrast[q], ")")
      #   }
      #   unadj.output.list[["p.val"]] <- ""
      #   #output[nrow(output) + 1,] = unadj.output.list ##uncomment to display pairwise comparisons in main output table/file
      #   output.pairwise[nrow(output.pairwise) + 1,] = unadj.output.list
      # 
      #   #adjusted
      #   if(isTRUE(adj)){
      #     adj.output.list <- list()
      #     adj.output.list[["#"]] <- "Pairwise adj"
      #     for(r in 1:grps) {
      #       adj.output.list[[group.names[r]]] <- paste0(em.pairwise.adj$p.value[r], " (", em.pairwise.adj$contrast[r], ")")
      #     }
      #     adj.output.list[["p.val"]] <- ""
      #     #output[nrow(output) + 1,] = adj.output.list ##uncomment to display pairwise comparisons in main output table/file
      #     output.pairwise[nrow(output.pairwise) + 1,] = adj.output.list
      #   }
      # }
      
      if(isTRUE(runpairwise)){

        
        if(is.null(output.pairwise)){
          output.pairwise <- em.pairwise
        } else {
          output.pairwise <- merge(output.pairwise, em.pairwise, all = T)
        }
        
        
        if(isTRUE(adj)){
          if(is.null(output.pairwise.adj)){
            output.pairwise.adj <- em.pairwise.adj
          } else {
            output.pairwise.adj <- merge(output.pairwise.adj, em.pairwise.adj, all = T)
          }
          
        } 
        
      }
      
      #calculate time elapsed and restart clock
      toc(); tic(); 
      cat(nl)
    }
    
    
    #write to file or return output to main function
    #if(isTRUE(demographics)){
    #  write.csv(output, file = paste(output_path, "/", outcomevar, filesuffix, "_Sum.csv", sep=""))
    
    #  if(isTRUE(runpairwise)){
    #    write.csv(output.pairwise, file = paste(output_path, "/", outcomevar, filesuffix, "_Sum_pairwise.csv", sep=""))
    #  }
    
    #} else {
    #  return(output)
    #}
    
    #write to file or return output to main function
    if(isTRUE(runpairwise)){
      # output.pairwise <- em.pairwise
      output.pairwise.title <- paste0("Pairwise comparisons for ", outcomevar, " on ", baselinevisit ,", ", toString.default(visitnumbers), " in the ",popn ," population")
      payload[[paste0("SUMS TITLE: ", outcomevar, filesuffix, "pairwise_unadj")]] <- paste0("Unadjusted,", output.pairwise.title)
      payload[[paste0("SUMS ", outcomevar, filesuffix, "pairwise_unadj")]] <- output.pairwise
      
      if(isTRUE(adj)){
        # output.pairwise.adj <- em.pairwise.adj
        payload[[paste0("SUMS TITLE (ADJ): ", outcomevar, filesuffix, "pairwise_adj")]] <- paste0("Adjusted,", output.pairwise.title)
        payload[[paste0("SUMS (ADJ) ", outcomevar, filesuffix, "pairwise_adj")]] <- output.pairwise.adj
      }
  
    }
    
    
    if(isTRUE(demographics)){
      # print(output)
      output <- RenameHeaders(output, demographics)
      # write.csv(output, file = paste(output_path, "/", outcomevar, filesuffix, "_Sum.csv", sep=""))
      output.title <- paste0(outcomevar, " on ", baselinevisit ,", ", toString.default(visitnumbers), " in the ",popn ," population")
      #cat("#===== SUMCombined TITLE", output.title, nl)
      payload[[paste0("TITLE: ", outcomevar, filesuffix)]] <- output.title
      payload[[paste0(outcomevar, filesuffix)]] <- output
      
      
      #Export to file 
      if(".csv" %in% exportfile){
        utils::write.csv(output, 
                         file = paste(exppath, outcomevar, filesuffix, ".csv", sep=""))
        if(isTRUE(runpairwise)){
          utils::write.csv(output.pairwise, 
                           file = paste(exppath, outcomevar, filesuffix, 
                                        "_Pairwise.csv", sep=""))
        }
      }
      if(".doc" %in% exportfile){
        print(exportpath)
        q.write.to.word(payload, exportpath=exportpath, 
                        docname=paste0(outcomevar, filesuffix))
        
        # if(isTRUE(runpairwise)){
        #   q.write.to.word(payload.pairwise, exportpath=exportpath, 
        #                   docname=paste0(outcomevar, filesuffix, "_Pairwise" ))
        # }
      }
      
      cat(green("v ") %+% blue("SUMMARY "), nl)
      # print(output.title)
      # print(payload)
      
      toc(); cat(nl);
      
      return(payload)
      
    } else {
      return(output)
    }
    
  }
  #
  #
  #
  #####2. Summarize Raw Data (Count data/Poisson distribution) #####
  SumsGLMCombined <- function( db, db.imp, outcomevar, baselinevisit, visitnumbers, filesuffix, demographics, runpairwise, adj, assume.normal.dist, 
                               useranks, ggroups, grps, group.names, output, output.pairwise, glmgeefamily=glmgeefamily, design){
    
    if(is.null(glmgeefamily)){
      print("#===== PLEASE SPECIFY FAMILY FOR GLM MODEL! ====#")
      return("#===== PLEASE SPECIFY FAMILY FOR GLM MODEL! ====#")
    }
    w <- db
    w <- db.imp #comment this out to generate descriptives using non-imputed dataset 
    w.imp <- db.imp
    
    
    #w <- db[ which( db$LBTESTH == testname),]
    viz <- c( baselinevisit, visitnumbers )
    
    #prepare output object
    #output <- data.frame(Group.A=character(), Group.B=character(), p.val=numeric(),stringsAsFactors=FALSE)
    
    
    
    #summarize data by visit
    s=1
    #bm.btwn.p <- NA
    #bm.pairwise <- data.frame(AB=NA, AC=NA, AD=NA, AE=NA, AF=NA, BC=NA, BD=NA, BE=NA, BF=NA, CD=NA, CE=NA, CF=NA, DE=NA, DF=NA, EF=NA)
    #bbm.pairwise <- data.frame()
    
    cat( nl, nl, nl, nl, "# ------- ------- SUMS  ------- -------- #", nl)
    for(i in viz){
      u <- w[which( w$VISITNUMBER == i ),]
      u.imp <- w.imp[which( w.imp$VISITNUMBER == i ),]
      
      
      famm=substr(toString(glmgeefamily), 1, 7)
      
      if(famm == "binomia"){
        #Logistic regression
        cat("# ------- ------- LOGISTIC REGRESSION - TIMEPOINT: ",i," ------- -------- #", nl)
        mysumm <- SumCatGEE(u, outcomevar="OUTCOME", ggroups=NULL, filesuffix )
        print(mysumm)
        
      } else {
        mysumm <- with( u, aggregate(OUTCOME, by=list(GROUP = GROUPING), FUN = f) )
        cat("# ------- ------- TIME POINT: ",i," ------- -------- #", nl)
        print(mysumm)
        
      }
      
      #s.test <- shapiro.test(u.imp$OUTCOME)$p.value
      #normality2(u.imp, u.imp$OUTCOME)
      #cat("Sums Shapiro Test 1: ", s.test, nl)
      #m <- lm(OUTCOME ~ GROUPING, data = u.imp, na.action = na.omit)
      #par(mfrow = c(2, 2))
      #plot(m)
      
      
      
      if(famm == "poisson"){
        
        if(design == "parallel"){
          assum.model <- glm(OUTCOME ~ GROUPING, data = u.imp, family = poisson(link = "log"), na.action = na.omit )
        } else if(design == "crossover"){
          assum.model <- glm(OUTCOME ~ GROUPING + SEQ + PERIOD, data = u.imp, family = poisson(link = "log"), na.action = na.omit )
        }
        
        od <- AER::dispersiontest(assum.model)
        od.test <- od$p.value
        cat(nl, "Over-dispersion Test1: ", od.test, nl)
      } else {
        od.test <- 1
        cat(nl, "No Over-dispersion Test Done: ", nl)
      }
      
      #assum.model <- glm(OUTCOME ~ GROUPING, data = u.imp, family = glmgeefamily, na.action = na.omit )
      
      
      if(od.test < 0.05){ 
        if(isTRUE(useranks)){
          u.imp$OUTCOME2 <- rank(u.imp$OUTCOME, ties.method="max")
          
          if(design == "parallel"){
            mysumm.m <- lm(OUTCOME2 ~ GROUPING, data = u.imp, na.action = na.omit )
          } else if(design == "crossover"){
            mysumm.m <- lm(OUTCOME2 ~ GROUPING + SEQ + PERIOD, data = u.imp, na.action = na.omit )
          }
          result <- try(car::Anova(mysumm.m))
          #print("#==== RANK result =====#")
          #print(result)
          p.val <- n_decimals(result$`Pr(>F)`[1], 3)
          p.val <- paste0(p.val, " (r)")
          
          
          
          if(isTRUE(runpairwise)){
            EM <- emmeans( mysumm.m, ~GROUPING )
            final.model2 <- update( EM, infer = c( TRUE, TRUE ))
            EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
            em.pairwise <- data.frame( EM5 )
            em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
            em.pairwise$timepoint <- i
            if(isTRUE(adj)){
              EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
              em.pairwise.adj <- data.frame( EM6 )
              em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
              em.pairwise.adj$timepoint <- i
            }
          }
          
        } else {
          if(length(ggroups) == 1){
            p.val <- NA
            
          } else if(length(ggroups) == 2){
            
            if(design == "parallel"){
              result <- wilcox.test(OUTCOME ~ GROUPING, data = u.imp, na.action = na.omit)
            } else if(design == "crossover"){
              result <- wilcox.test(OUTCOME ~ GROUPING + SEQ + PERIOD, data = u.imp, na.action = na.omit)
            }
            #p.val <- n_decimals(result$p.value, 3)
            p.val <- paste0(result$p.value, " (w)")
            
          } else if(length(ggroups) > 2){
            result <- kruskal.test(OUTCOME ~ GROUPING, data = u.imp, na.action = na.omit)
            p.val <- paste0(result$p.value, " (k)")
            
            if(isTRUE(runpairwise)){
              posthoc <- PMCMRplus::kwAllPairsNemenyiTest(OUTCOME ~ GROUPING, data = u.imp, p.adjust.method=NULL)
              print(posthoc)
              
              posthoc1 <- as.matrix(posthoc$p.value)
              posthoc2 <- as.data.frame(as.table(posthoc1))
              posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
              posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
              posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
              em.pairwise <- posthoc3[c("contrast", "p.value")]
              # print(em.pairwise)
              em.pairwise$timepoint <- i
              
              if(isTRUE(adj)){
                em.pairwise.adj <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("contrast", "p.value") )
                posthoc <- PMCMRplus::kwAllPairsNemenyiTest(OUTCOME ~ GROUPING, data = u.imp, p.adjust.method="single-step")
                print(posthoc)
                posthoc1 <- as.matrix(posthoc$p.value)
                posthoc2 <- as.data.frame(as.table(posthoc1))
                posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
                posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
                posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
                em.pairwise.adj <- posthoc3[c("contrast", "p.value")]
                # print(em.pairwise.adj)
                em.pairwise.adj$timepoint <- i
              }
              
            }
            
          }
          s=s+1
        }
      } else {
        
        if(famm=="binomia"){
          #Logistic regression
          
          if(design == "parallel"){
            logit.mod <-glm(OUTCOME ~ GROUPING, data = u.imp, family = "binomial", na.action = na.omit)
          } else if(design == "crossover"){
            logit.mod <-glm(OUTCOME ~ GROUPING + SEQ + PERIOD, data = u.imp, family = "binomial", na.action = na.omit)
          }
          print(summary(logit.mod))
          
          #em.btwn <- wald.test(b = coef(logit.mod), Sigma = vcov(logit.mod), Terms = 2:grps)
          #em.btwn.matrix <- as.matrix(em.btwn)
          #em.btwn.df <- as.data.frame(em.btwn$result)
          #p.val <- em.btwn.df["P",1]
          #p.val <- n_decimals(p.val, 3)
          #cat("#==== logistic p-value =====#", nl, p.val, nl)
          
          
          EM1 <- emmeans( logit.mod, ~GROUPING )
          logit.mod2 <- update( EM1, infer = c( TRUE, TRUE ))
          EM4 <- joint_tests( logit.mod2 ) ## Gives same result as em.btwn <- wald.test(b = coef(logit.mod), Sigma = vcov(logit.mod), Terms = 2:grps)
          em.btwn4 <- data.frame( EM4 )
          print("#==== logistic em.btwn =====#")
          print(em.btwn4)
          p.val <- n_decimals(em.btwn4$p.value[em.btwn4$model.term == "GROUPING"], 3)
          cat("#==== logistic p-value =====#", nl, p.val, nl)
          
          
          if(isTRUE(runpairwise)){
            EM <- emmeans( logit.mod, ~GROUPING )
            final.model2 <- update( EM, infer = c( TRUE, TRUE ))
            EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
            em.pairwise <- data.frame( EM5 )
            em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
            em.pairwise$timepoint <- i
            if(isTRUE(adj)){
              EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
              em.pairwise.adj <- data.frame( EM6 )
              em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
              em.pairwise.adj$timepoint <- i
            }
          }
          
          
          
          
          
          
        } else{
          #glmer.model <- glmer(OUTCOME ~ GROUPING + (1|SUBJECTNUM), data = u.imp, family = poisson(link = "log"), na.action = na.omit )
          glmer.model <- lme4::glmer(OUTCOME ~ GROUPING + (1|SUBJECTNUM), data = u.imp, family = glmgeefamily, na.action = na.omit )
          if(design == "parallel"){
            glmer.model <- lme4::glmer(OUTCOME ~ GROUPING + (1|SUBJECTNUM), data = u.imp, family = glmgeefamily, na.action = na.omit )
          } else if(design == "crossover"){
            glmer.model <- lme4::glmer(OUTCOME ~ GROUPING + SEQ + PERIOD + (1|SUBJECTNUM), data = u.imp, family = glmgeefamily, na.action = na.omit )
          }
          
          EM <- emmeans( glmer.model, ~GROUPING )
          glmer.model2 <- update( EM, infer = c( TRUE, TRUE ))
          EM4 <- joint_tests( glmer.model2 )
          em.btwn <- data.frame( EM4 )
          print("#==== em.btwn =====#")
          print(em.btwn)
          p.val <- n_decimals(em.btwn$p.value[em.btwn$model.term == "GROUPING"], 3)
          s=s+1
          
          if(isTRUE(runpairwise)){
            EM <- emmeans( glmer.model, ~GROUPING )
            final.model2 <- update( EM, infer = c( TRUE, TRUE ))
            EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
            em.pairwise <- data.frame( EM5 )
            em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
            em.pairwise$timepoint <- i
            if(isTRUE(adj)){
              EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
              em.pairwise.adj <- data.frame( EM6 )
              em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
              em.pairwise.adj$timepoint <- i
            }
          }
        }
        
      }
      
      
      the.p.val <- as.character(p.val)
      if(substring_right(the.p.val, 3) %in% c("(l)", "(r)", "(w)", "(k)")){
        filesuffix <- substring_right(the.p.val, 3)
        a <- substr(the.p.val, 1, 6)
        a <- as.numeric(a)
        p.val <- n_decimals(a, 3)
        p.val <- ifelse(p.val == "1" | p.val == 1, "1.000", p.val)
        p.val <- paste0(p.val, " ", filesuffix)
      } else {
        a <- as.numeric(the.p.val)
        p.val <- n_decimals(a, 3)
        p.val <- as.character(p.val)
        p.val <- ifelse(p.val == "1" | p.val == 1, "1.000", p.val)
      }
      
      # Output things ####
      #table header
      #header.output.list <- list()
      #header.output.list[["#"]] <- outcomevar
      #for(head_item in 1:grps) {
      #  header.output.list[[group.names[head_item]]] <- ""
      #}
      #header.output.list[["p.val"]] <- ""
      #output[nrow(output) + 1,] = header.output.list
      #output.pairwise[nrow(output.pairwise) + 1,] = header.output.list
      
      if(famm=="binomia"){
        #output things for logistic
        mysumm$p.val[1] <- p.val
        output <- rbind.data.frame(output, mysumm)
        print("#===== BINOMIAL OUTPUT i ====#")
        print(output)
        
        # if(isTRUE(runpairwise)){
        #   #output.pairwise[nrow(output.pairwise) + 1,] = list(paste0("Visit ", i))
        #   output.pairwise[nrow(output.pairwise) + 1,] = list(paste0(i))
        #   print("#==== Binomial em.pairwise =====#")
        #   print(em.pairwise)
        #   #unadjusted
        #   unadj.output.list <- list()
        #   unadj.output.list[["#"]] <- "Pairwise unadj"
        #   for(q in 1:grps) {
        #     unadj.output.list[[group.names[q]]] <- paste0(em.pairwise$p.value[q], " (", em.pairwise$contrast[q], ")")
        #   }
        #   unadj.output.list[["p.val"]] <- ""
        #   #output[nrow(output) + 1,] = unadj.output.list ##uncomment to display pairwise comparisons in main output table/file
        #   output.pairwise[nrow(output.pairwise) + 1,] = unadj.output.list
        #   
        #   if(isTRUE(adj)){
        #     #adjusted
        #     adj.output.list <- list()
        #     adj.output.list[["#"]] <- "Pairwise adj"
        #     for(r in 1:grps) {
        #       adj.output.list[[group.names[r]]] <- paste0(em.pairwise.adj$p.value[r], " (", em.pairwise.adj$contrast[r], ")")
        #     }
        #     adj.output.list[["p.val"]] <- ""
        #     #output[nrow(output) + 1,] = adj.output.list ##uncomment to display pairwise comparisons in main output table/file
        #     output.pairwise[nrow(output.pairwise) + 1,] = adj.output.list
        #   }
        # }
        
      } else {
        
        #means & SDs
        ms.output.list <- list()
        #ms.output.list[["#"]] <- paste0("Visit ", i)
        ms.output.list[["#"]] <- paste0(i)
        for(j in 1:grps) {
          ms.output.list[[group.names[j]]] <- paste0( n_decimals(mysumm$x[j,5], 2), " \u00B1 ", n_decimals(mysumm$x[j,6], 2), " (",mysumm$x[j,1],")" )
        }
        ms.output.list[["p.val"]] <- p.val
        output[nrow(output) + 1,] = ms.output.list
        
        #medians & ranges
        mr.output.list <- list()
        mr.output.list[["#"]] <- " "
        for(m in 1:grps) {
          mr.output.list[[group.names[m]]] <- paste0( n_decimals(mysumm$x[m,2], 2), " (", n_decimals(mysumm$x[m,3], 2), " to ", n_decimals(mysumm$x[m,4], 2), ")" )
        }
        mr.output.list[["p.val"]] <- " "
        output[nrow(output) + 1,] = mr.output.list
        
        
        # if(isTRUE(runpairwise)){
        #   #output.pairwise[nrow(output.pairwise) + 1,] = list(paste0("Visit ", i))
        #   output.pairwise[nrow(output.pairwise) + 1,] = list(paste0(i))
        #   #print("#==== em.pairwise =====#")
        #   #print(em.pairwise)
        #   #unadjusted
        #   unadj.output.list <- list()
        #   unadj.output.list[["#"]] <- "Pairwise unadj"
        #   for(q in 1:grps) {
        #     unadj.output.list[[group.names[q]]] <- paste0(em.pairwise$p.value[q], " (", em.pairwise$contrast[q], ")")
        #   }
        #   unadj.output.list[["p.val"]] <- ""
        #   #output[nrow(output) + 1,] = unadj.output.list ##uncomment to display pairwise comparisons in main output table/file
        #   output.pairwise[nrow(output.pairwise) + 1,] = unadj.output.list
        #   
        #   if(isTRUE(adj)){
        #     #adjusted
        #     adj.output.list <- list()
        #     adj.output.list[["#"]] <- "Pairwise adj"
        #     for(r in 1:grps) {
        #       adj.output.list[[group.names[r]]] <- paste0(em.pairwise.adj$p.value[r], " (", em.pairwise.adj$contrast[r], ")")
        #     }
        #     adj.output.list[["p.val"]] <- ""
        #     #output[nrow(output) + 1,] = adj.output.list ##uncomment to display pairwise comparisons in main output table/file
        #     output.pairwise[nrow(output.pairwise) + 1,] = adj.output.list
        #   }
        # }
        
        
        
      }
      
      if(isTRUE(runpairwise)){
        
        
        if(is.null(output.pairwise)){
          output.pairwise <- em.pairwise
        } else {
          output.pairwise <- merge(output.pairwise, em.pairwise, all = T)
        }
        
        
        if(isTRUE(adj)){
          if(is.null(output.pairwise.adj)){
            output.pairwise.adj <- em.pairwise.adj
          } else {
            output.pairwise.adj <- merge(output.pairwise.adj, em.pairwise.adj, all = T)
          }
          
        } 
        
      }
      
    }
    
    
    
    
  
  #write to file or return output to main function
  if(isTRUE(runpairwise)){
    # output.pairwise <- em.pairwise
    output.pairwise.title <- paste0("Pairwise comparisons for ", outcomevar, " on ", baselinevisit ,", ", toString.default(visitnumbers), " in the ",popn ," population")
    payload[[paste0("GLM TITLE: ", outcomevar, filesuffix, "pairwise_unadj")]] <- paste0("Unadjusted,", output.pairwise.title)
    payload[[paste0("GLM ", outcomevar, filesuffix, "pairwise_unadj")]] <- output.pairwise
    
    if(isTRUE(adj)){
      # output.pairwise.adj <- em.pairwise.adj
      payload[[paste0("GLM TITLE (ADJ): ", outcomevar, filesuffix, "pairwise_adj")]] <- paste0("Adjusted,", output.pairwise.title)
      payload[[paste0("GLM (ADJ) ", outcomevar, filesuffix, "pairwise_adj")]] <- output.pairwise.adj
    }
    
  }
  
  
  if(isTRUE(demographics)){
    # print(output)
    output <- RenameHeaders(output, demographics)
    # write.csv(output, file = paste(output_path, "/", outcomevar, filesuffix, "_Sum.csv", sep=""))
    output.title <- paste0(outcomevar, " on ", baselinevisit ,", ", toString.default(visitnumbers), " in the ",popn ," population")
    #cat("#===== SUMCombined TITLE", output.title, nl)
    payload[[paste0("GLM TITLE: ", outcomevar, filesuffix)]] <- output.title
    payload[[paste0(outcomevar, filesuffix)]] <- output
    
    
    #Export to file 
    if(".csv" %in% exportfile){
      utils::write.csv(output, 
                       file = paste(exppath, outcomevar, filesuffix, ".csv", sep=""))
      if(isTRUE(runpairwise)){
        utils::write.csv(output.pairwise, 
                         file = paste(exppath, outcomevar, filesuffix, 
                                      "_Pairwise.csv", sep=""))
      }
    }
    if(".doc" %in% exportfile){
      print(exportpath)
      q.write.to.word(payload, exportpath=exportpath, 
                      docname=paste0(outcomevar, filesuffix))
      
      # if(isTRUE(runpairwise)){
      #   q.write.to.word(payload.pairwise, exportpath=exportpath, 
      #                   docname=paste0(outcomevar, filesuffix, "_Pairwise" ))
      # }
    }
    
    cat(green("v ") %+% blue("SUMMARY "), nl)
    # print(output.title)
    # print(payload)
    
    toc(); cat(nl);
    
    return(payload)
    
  } else {
    return(output)
  }
    
    
    
  }
  #
  #
  #
  #
  #####3. Summarize Raw Data (Count data/Poisson distribution) #####
  SumsGEECombined <- function( db, db.imp, outcomevar, baselinevisit, visitnumbers, filesuffix, demographics, runpairwise, adj, assume.normal.dist, 
                               useranks, ggroups, grps, group.names, output, output.pairwise, glmgeefamily=glmgeefamily, design ){
    
    if(is.null(glmgeefamily)){
      return("#===== PLEASE SPECIFY FAMILY FOR GEE MODEL! ====#")
    }
    w <- db
    w.imp <- db.imp
    
    viz <- c( baselinevisit, visitnumbers )
    
    #summarize data by visit
    s=1
    
    cat( nl, nl, nl, nl, "# ------- ------- SUMS  ------- -------- #", nl)
    for(i in viz){
      u <- w[which( w$VISITNUMBER == i ),]
      u.imp <- w.imp[which( w.imp$VISITNUMBER == i ),]
      
      
      
      cat("# ------- ------- TIME POINT: ",i," ------- -------- #", nl)
      sumcat <- SumCatGEE(u, outcomevar="OUTCOME", ggroups=NULL, filesuffix )
      print(sumcat)
      
      
      if(design == "parallel"){
        gee.model <- gee::gee(OUTCOME ~ GROUPING,  id = u.imp$SUBJECTNUM,
                         data = u.imp, family = glmgeefamily,
                         corstr = "exchangeable", scale.fix = TRUE,
                         scale.value = 1, na.action = na.omit)
      } else if(design == "crossover"){
        gee.model <- gee::gee(OUTCOME ~ GROUPING + SEQ + PERIOD,  id = u.imp$SUBJECTNUM,
                         data = u.imp, family = glmgeefamily, 
                         corstr = "exchangeable", scale.fix = TRUE,
                         scale.value = 1, na.action = na.omit)
      }
      
      EM <- emmeans( gee.model, ~GROUPING )
      gee.model2 <- update( EM, infer = c( TRUE, TRUE ))
      EM4 <- joint_tests( gee.model2 )
      em.btwn <- data.frame( EM4 )
      p.val <- n_decimals(em.btwn$p.value[1], 3)
      
      if(isTRUE(runpairwise)){
        EM2 <- emmeans( gee.model, ~GROUPING )
        EM5 <- contrast( EM2, "pairwise", adjust = NULL )
        em.pairwise <- data.frame( EM5 )
        em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
        em.pairwise$timepoint <- i
        if(isTRUE(adj)){
          EM6 <- contrast( EM2, "pairwise", adjust = "tukey" )
          em.pairwise.adj <- data.frame( EM6 )
          em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
          em.pairwise.adj$timepoint <- i
        }
      }
      
      
      the.p.val <- as.character(p.val)
      if(substring_right(the.p.val, 3) %in% c("(l)", "(r)", "(w)", "(k)")){
        filesuffix <- substring_right(the.p.val, 3)
        a <- substr(the.p.val, 1, 6)
        a <- as.numeric(a)
        p.val <- n_decimals(a, 3)
        p.val <- paste0(p.val, " ", filesuffix)
      } else {
        a <- as.numeric(the.p.val)
        p.val <- n_decimals(a, 3)
        p.val <- as.character(p.val)
      }
      
      #output things 
      sumcat$p.val[1] <- p.val
      output <- rbind.data.frame(output, sumcat)
      cat("#===== GEE OUTPUT AT TIMEPOINT: ", i, " ====#")
      print(output)
      
      
      # if(isTRUE(runpairwise)){
      #   #output.pairwise[nrow(output.pairwise) + 1,] = list(paste0("Visit ", i))
      #   output.pairwise[nrow(output.pairwise) + 1,] = list(paste0(i))
      #   
      #   #("#===== EM-PAIRWISE ====#")
      #   #print(em.pairwise)
      #   #unadjusted
      #   unadj.output.list <- list()
      #   unadj.output.list[["#"]] <- "Pairwise unadj"
      #   for(q in 1:grps) {
      #     unadj.output.list[[group.names[q]]] <- paste0(em.pairwise$p.value[q], " (", em.pairwise$contrast[q], ")")
      #   }
      #   unadj.output.list[["p.val"]] <- ""
      #   output.pairwise[nrow(output.pairwise) + 1,] = unadj.output.list
      #   
      #   #adjusted      
      #   if(isTRUE(adj)){
      #     print("#===== ADJUSTED EM-PAIRWISE ====#")
      #     print(em.pairwise.adj)
      #     adj.output.list <- list()
      #     adj.output.list[["#"]] <- "Pairwise adj"
      #     for(r in 1:grps) {
      #       adj.output.list[[group.names[r]]] <- paste0(em.pairwise.adj$p.value[r], " (", em.pairwise.adj$contrast[r], ")")
      #     }
      #     adj.output.list[["p.val"]] <- ""
      #     output.pairwise[nrow(output.pairwise) + 1,] = adj.output.list
      #   }
      # }
      
      if(isTRUE(runpairwise)){
        
        
        if(is.null(output.pairwise)){
          output.pairwise <- em.pairwise
        } else {
          output.pairwise <- merge(output.pairwise, em.pairwise, all = T)
        }
        
        
        if(isTRUE(adj)){
          if(is.null(output.pairwise.adj)){
            output.pairwise.adj <- em.pairwise.adj
          } else {
            output.pairwise.adj <- merge(output.pairwise.adj, em.pairwise.adj, all = T)
          }
          
        } 
        
      }
    }
    
    
    
    
  
  
  
  
  
  
  #write to file or return output to main function
  if(isTRUE(runpairwise)){
    # output.pairwise <- em.pairwise
    output.pairwise.title <- paste0("Pairwise comparisons for ", outcomevar, " on ", baselinevisit ,", ", toString.default(visitnumbers), " in the ",popn ," population")
    payload[[paste0("GEE TITLE: ", outcomevar, filesuffix, "pairwise_unadj")]] <- paste0("Unadjusted,", output.pairwise.title)
    payload[[paste0("GEE ", outcomevar, filesuffix, "pairwise_unadj")]] <- output.pairwise
    
    if(isTRUE(adj)){
      # output.pairwise.adj <- em.pairwise.adj
      payload[[paste0("GEE TITLE (ADJ): ", outcomevar, filesuffix, "pairwise_adj")]] <- paste0("Adjusted,", output.pairwise.title)
      payload[[paste0("GEE (ADJ) ", outcomevar, filesuffix, "pairwise_adj")]] <- output.pairwise.adj
    }
    
  }
  
  
  if(isTRUE(demographics)){
    # print(output)
    output <- RenameHeaders(output, demographics)
    # write.csv(output, file = paste(output_path, "/", outcomevar, filesuffix, "_Sum.csv", sep=""))
    output.title <- paste0(outcomevar, " on ", baselinevisit ,", ", toString.default(visitnumbers), " in the ",popn ," population")
    #cat("#===== SUMCombined TITLE", output.title, nl)
    payload[[paste0("GEE TITLE: ", outcomevar, filesuffix)]] <- output.title
    payload[[paste0(outcomevar, filesuffix)]] <- output
    
    
    #Export to file 
    if(".csv" %in% exportfile){
      utils::write.csv(output, 
                       file = paste(exppath, outcomevar, filesuffix, ".csv", sep=""))
      if(isTRUE(runpairwise)){
        utils::write.csv(output.pairwise, 
                         file = paste(exppath, outcomevar, filesuffix, 
                                      "_Pairwise.csv", sep=""))
      }
    }
    if(".doc" %in% exportfile){
      print(exportpath)
      q.write.to.word(payload, exportpath=exportpath, 
                      docname=paste0(outcomevar, filesuffix))
      
      # if(isTRUE(runpairwise)){
      #   q.write.to.word(payload.pairwise, exportpath=exportpath, 
      #                   docname=paste0(outcomevar, filesuffix, "_Pairwise" ))
      # }
    }
    
    cat(green("v ") %+% blue("SUMMARY "), nl)
    # print(output.title)
    # print(payload)
    
    toc(); cat(nl);
    return(payload)
    
    } else {
      return(output)
    }
  }
  #
  #
  #
  #
  #####4. Summarize change in outcome variable #####
  ChangeSumsCombined <- function(db, db.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, 
                                 filesuffix, runpairwise, adj, withinpairs, covs, normoverride, useranks, 
                                 ggroups, grps, group.names, output, output.pairwise, output.pairwise.adj, design ){
    
    w.noimp <- db
    w.noimp <- db.imp #comment this out to generate descriptives using non-imputed dataset 
    w.imp <- db.imp
    #return(w.noimp)
    
    if(!is.na(speccomp1) & !is.na(speccomp2)){
      visitnumbers = c( visitnumbers, paste0(speccomp1, speccomp2))
    }
    #print(visitnumbers)
    #w <- db[ which( db$LBTESTH == testname),]
    
    #prepare output object and other stuff
    #big.p.val <- list("","","")
    output.pchange <- setNames(data.frame(matrix(ncol = grps, nrow = 0)), c(group.names) )
    em.within <- data.frame(GROUPING = numeric(), VISITNUMBER = numeric(), emmean = numeric(), SE = numeric(), 
                            df = numeric(), lower.CL = numeric(), upper.CL = numeric(), t.ratio = numeric(),
                            p.value = character(), stringsAsFactors = FALSE)
    
    ## Check covariance matrices ##
    #w <- w.imp[!is.na(w.imp$CHANGE), ]
    #w <- w[w$VISITNUMBER != baselinevisit, ]
    #print(summary(w)
    
    
    
    ## Descriptives ####
    #k = 1 #for lsmeans cells
    #l = 1 #for between group p-values
    #cat("Visit Numbers: ", visitnumbers, nl)
    for(i in visitnumbers){
      #u <- w.noimp[which( w.noimp$VISITNUMBER == i & !is.na(w.imp$CHANGE) ),]
      u <- w.noimp[which( w.noimp$VISITNUMBER == i ),]
      
      #return(u)
      
      #u.imp <- w.imp[which( w.imp$VISITNUMBER == i & !is.na(w.imp$CHANGE) ),]
      w <- u.imp <- w.imp[which( w.imp$VISITNUMBER == i ),]
      #return(u.imp)
      chgsumm <- with( u, aggregate(CHANGE, by=list(GROUP = GROUPING), FUN = f) )
      # percent.change <- with( u, aggregate(PERCENT, by=list(GROUP = GROUPING), FUN = f) )
      #percent.change <- q.desc_stats(u, groupvar="GROUPING", outcomevar="PERCENT")
      #cat("# ---------- CHANGE VISIT ",i," ----------- #", nl)
      cat(green("v ") %+% blue("CHANGE AT TIMEPOINT: " %+% as.character(i)), nl)
      #print(chgsumm)
      #cat("Percent Change:", nl)
      #print(percent.change)
      #stat.desc(u)
      
      
      ##### Model things #####
      if(length(ggroups) == 1){
        em.btwn <- setNames(data.frame(matrix(ncol = 1, nrow = 1)), c("p.value") )
        em.btwn$p.value[1] <- NA
        
      } else if(length(ggroups) > 1){
        
        if(design == "parallel"){
          ## Check covariance structures for parammel design parallel##
          csxx <- cshx <- ar1x <- arh1 <- plai <- pla2 <- list()
          
          if(substr(covs, 1, 1)=="T"){
            fit.csxx <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE, random= ~1|SUBJECTNUM, 
                                     cor=corCompSymm(), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
            ifelse(!is.null(fit.csxx), csxx <- summary(fit.csxx), csxx$AIC <- NA)
            csxx$AIC
          } else {
            csxx$AIC <- NA
          }
          
          if(substr(covs, 2, 2)=="T"){
            fit.cshx <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE, random=~1|SUBJECTNUM, 
                                     cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
            #cshx <- summary(fit.cshx)
            ifelse(!is.null(fit.cshx), cshx <- summary(fit.cshx), cshx$AIC <- NA)
            cshx$AIC
          } else {
            cshx$AIC <- NA
          }
          
          if(substr(covs, 3, 3)=="T"){
            fit.ar1x <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE, random= ~1|SUBJECTNUM, 
                                     cor=corAR1(), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
            
            #ar1x <- summary(fit.ar1x)
            ifelse(!is.null(fit.ar1x), ar1x <- summary(fit.ar1x), ar1x$AIC <- NA)
            ar1x$AIC
          } else {
            ar1x$AIC <- NA
          }
          
          if(substr(covs, 4, 4)=="T"){
            fit.arh1 <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE, random=~1|SUBJECTNUM, 
                                     corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
            #arh1 <- summary(fit.arh1)
            ifelse(!is.null(fit.arh1), arh1 <- summary(fit.arh1), arh1$AIC <- NA)
            arh1$AIC
          } else {
            arh1$AIC <- NA
          }
          if(substr(covs, 5, 5)=="T"){
            fit.plai <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE, random=~1|SUBJECTNUM, 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
            ifelse(!is.null(fit.plai), plai <- summary(fit.plai), plai$AIC <- NA)
            plai$AIC
          } else {
            plai$AIC <- NA
          }
          if(substr(covs, 6, 6)=="T"){ 
            fit.pla2 <- with( w, lm(CHANGE ~ GROUPING + BASELINE))
            #ifelse(!is.null(fit.pla2), pla2 <- summary(fit.pla2), pla2$AIC <- NA)
            ifelse(!is.null(fit.pla2), pla2$AIC <- extractAIC(fit.pla2)[2], pla2$AIC <- NA)
            pla2$AIC
            
            pla2$AIC
          } else {
            pla2$AIC <- NA
          }
          
          
          aics <- data.frame(csxx$AIC, cshx$AIC, ar1x$AIC, arh1$AIC, plai$AIC, pla2$AIC)
          m.name <- names(aics)[which.min(apply(aics,MARGIN=2,min))]
          f.model <- paste0("fit.", substr(m.name, 1, 4))
          #cat("Final Covariance Structure: ", f.model, nl)
          cat(silver("  Final covariance structure for non-transformed data: " %+% f.model), nl)
          
        } else if(design == "crossover"){
          ## Check covariance structures for crossover##
          csxx <- cshx <- ar1x <- arh1 <- plai <- pla2 <- list()
          
          if(substr(covs, 1, 1)=="T"){
            fit.csxx <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                     cor=corCompSymm(), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
            ifelse(!is.null(fit.csxx), csxx <- summary(fit.csxx), csxx$AIC <- NA)
            csxx$AIC
          } else {
            csxx$AIC <- NA
          }
          
          if(substr(covs, 2, 2)=="T"){
            fit.cshx <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                     cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
            #cshx <- summary(fit.cshx)
            ifelse(!is.null(fit.cshx), cshx <- summary(fit.cshx), cshx$AIC <- NA)
            cshx$AIC
          } else {
            cshx$AIC <- NA
          }
          
          if(substr(covs, 3, 3)=="T"){
            fit.ar1x <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                     cor=corAR1(), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
            
            #ar1x <- summary(fit.ar1x)
            ifelse(!is.null(fit.ar1x), ar1x <- summary(fit.ar1x), ar1x$AIC <- NA)
            ar1x$AIC
          } else {
            ar1x$AIC <- NA
          }
          
          if(substr(covs, 4, 4)=="T"){
            fit.arh1 <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                     corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
            #arh1 <- summary(fit.arh1)
            ifelse(!is.null(fit.arh1), arh1 <- summary(fit.arh1), arh1$AIC <- NA)
            arh1$AIC
          } else {
            arh1$AIC <- NA
          }
          if(substr(covs, 5, 5)=="T"){
            fit.plai <- with( w, nlme::lme(CHANGE ~ GROUPING + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                     control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
            ifelse(!is.null(fit.plai), plai <- summary(fit.plai), plai$AIC <- NA)
            plai$AIC
          } else {
            plai$AIC <- NA
          }
          if(substr(covs, 6, 6)=="T"){ 
            fit.pla2 <- with( w, lm(CHANGE ~ GROUPING + BASELINE + SEQ + PERIOD))
            #ifelse(!is.null(fit.pla2), pla2 <- summary(fit.pla2), pla2$AIC <- NA)
            ifelse(!is.null(fit.pla2), pla2$AIC <- extractAIC(fit.pla2)[2], pla2$AIC <- NA)
            pla2$AIC
            
            pla2$AIC
          } else {
            pla2$AIC <- NA
          }
          
          
          aics <- data.frame(csxx$AIC, cshx$AIC, ar1x$AIC, arh1$AIC, plai$AIC, pla2$AIC)
          m.name <- names(aics)[which.min(apply(aics,MARGIN=2,min))]
          f.model <- paste0("fit.", substr(m.name, 1, 4))
          cat("Final Covariance Structure: ", f.model, nl)
        }
        
        #check for normality
        lev.test <- F #perform Levene's test?
        l.test <- NA
        final.model <- NA
        if (f.model == "fit.csxx"){
          final.model <- fit.csxx
        } 
        else if(f.model == "fit.cshx") {
          final.model <- fit.cshx
        } 
        else if(f.model == "fit.ar1x") {
          final.model <- fit.ar1x
        } 
        else if(f.model == "fit.arh1") {
          final.model <- fit.arh1
        } 
        else if(f.model == "fit.plai") {
          final.model <- fit.plai
          lev.test <- T
        } 
        else if(f.model == "fit.pla2") {
          final.model <- fit.pla2
          lev.test <- T
        } 
        else {
          return("Cant find model")
        }
        
        
        if(isTRUE(normoverride)){
          
          
          if(design == "parallel"){
            EM <- emmeans( final.model, ~GROUPING )
          } else if(design == "crossover"){
            EM <- emmeans( final.model, ~GROUPING + SEQ + PERIOD)
          }
          #return(em.within)
          final.model2 <- update( EM, infer = c( TRUE, TRUE ))
          #em.within <- data.frame( final.model2 )
          #return(em.within)
          EM4 <- joint_tests( final.model2 )
          em.btwn <- data.frame( EM4 )
          #print("Normoverride Btwn p-value: ")
          #print(em.btwn)
          ## pairwise comparisons 
          if(isTRUE(runpairwise)){
            #EM5 <- contrast(final.model2, "consec")
            EM <- emmeans( final.model, ~GROUPING )
            final.model2 <- update( EM, infer = c( TRUE, TRUE ))
            EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
            em.pairwise <- data.frame( EM5 )
            em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
            if(isTRUE(adj)){
              EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
              em.pairwise.adj <- data.frame( EM6 )
              em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
            }
          }
        } 
        else {
          
          s.test <- try(shapiro.test(resid(final.model))$p.value)
          if(isTRUE(lev.test)){
            #assum.m <- lm(CHANGE ~ GROUPING, data=w)
            #l.test <- try(leveneTest(assum.m)$`Pr(>F)`[1])
          } else {
            #l.test <- 1
          }
          #cat("Change Shapiro Test 1: ", s.test, nl)
          #cat("Change Levene's Test 1: ", l.test, nl, nl)
          if(exists("s.test")) cat(silver("  Shapiro Test for non-transformed data: " %+% as.character(s.test)), nl)
          
          
          if( s.test < 0.01 ){
            if(isTRUE(useranks)){
              w$OUTCOME2 <- rank(w$CHANGE, ties.method="average")
              
              if(design == "parallel"){
                rank.m <- lm(OUTCOME2 ~ GROUPING + BASELINE, data = w, na.action = na.omit )
              } else if(design == "crossover"){
                rank.m <- lm(OUTCOME2 ~ GROUPING + BASELINE + SEQ + PERIOD, data = w, na.action = na.omit )
              }
              #result <- Anova(rank.m)
              #p.val <- n_decimals(result$`Pr(>F)`[1], 3)
              #p.val <- paste0(p.val, " (r)")
              
              if(design == "parallel"){
                EM <- emmeans( rank.m, ~GROUPING )
              } else if(design == "crossover"){
                EM <- emmeans( rank.m, ~GROUPING + SEQ + PERIOD )
              }
              final.model2 <- update( EM, infer = c( TRUE, TRUE ))
              EM4 <- joint_tests( final.model2 )
              #print("Ranked ANOVA: ")
              #print(EM4)
              em.btwn <- data.frame( EM4 )
              #em.btwn$p.value[l] <- paste0(em.btwn$p.value[l], " (r)")
              em.btwn$p.value <- ifelse(is.na(em.btwn$p.value), NA, paste0(em.btwn$p.value, " (r)"))
              if(isTRUE(runpairwise)){
                EM <- emmeans( rank.m, ~GROUPING )
                final.model2 <- update( EM, infer = c( TRUE, TRUE ))
                EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
                em.pairwise <- data.frame( EM5 )
                em.pairwise$p.value <- paste0(n_decimals(em.pairwise$p.value, 3), " (r)")
                if(isTRUE(adj)){
                  EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
                  em.pairwise.adj <- data.frame( EM6 )
                  em.pairwise.adj$p.value <- paste0(n_decimals(em.pairwise.adj$p.value, 3), " (r)")
                }
                #print("Pairwise unajusted:")
                #print(em.pairwise)
              }
            } else {
              
              #if( s.test < 0.91 | l.test < 0.05 ){
              w$CHANGE1 <- log(w$CHANGE + (1 - min(w$CHANGE, na.rm = TRUE)))
              
              cat("CHANGE1 Length: ", length(w$CHANGE1), nl, nl)
              log.csxx <- log.cshx <- log.ar1x <- log.arh1 <- log.plai <- log.pla2 <- list()
              if(design == "parallel"){
                if(substr(covs, 1, 1)=="T"){
                  log.fit.csxx <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE, random= ~1|SUBJECTNUM, 
                                               cor=corCompSymm(), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
                  ifelse(!is.null(log.fit.csxx), log.csxx <- summary(log.fit.csxx), log.csxx$AIC <- NA)
                  log.csxx$AIC
                  
                } else {
                  log.csxx$AIC <- NA
                }
                
                if(substr(covs, 2, 2)=="T"){
                  log.fit.cshx <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE, random=~1|SUBJECTNUM, 
                                               cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
                  #cshx <- summary(log.fit.cshx)
                  ifelse(!is.null(log.fit.cshx), log.cshx <- summary(log.fit.cshx), log.cshx$AIC <- NA)
                  log.cshx$AIC
                } else {
                  log.cshx$AIC <- NA
                }
                
                if(substr(covs, 3, 3)=="T"){
                  log.fit.ar1x <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE, random= ~1|SUBJECTNUM, 
                                               cor=corAR1(), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
                  
                  #ar1x <- summary(log.fit.ar1x)
                  ifelse(!is.null(log.fit.ar1x), log.ar1x <- summary(log.fit.ar1x), log.ar1x$AIC <- NA)
                  log.ar1x$AIC
                } else {
                  log.ar1x$AIC <- NA
                }
                
                if(substr(covs, 4, 4)=="T"){
                  log.fit.arh1 <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE, random=~1|SUBJECTNUM, 
                                               corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
                  #arh1 <- summary(log.fit.arh1)
                  ifelse(!is.null(log.fit.arh1), log.arh1 <- summary(log.fit.arh1), log.arh1$AIC <- NA)
                  log.arh1$AIC
                } else {
                  log.arh1$AIC <- NA
                }
                if(substr(covs, 5, 5)=="T"){
                  log.fit.plai <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE, random=~1|SUBJECTNUM, 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
                  ifelse(!is.null(log.fit.plai), log.plai <- summary(log.fit.plai), log.plai$AIC <- NA)
                  log.plai$AIC
                } else {
                  log.plai$AIC <- NA
                }
                if(substr(covs, 6, 6)=="T"){ ##LME4 package
                  log.fit.pla2 <- with( w, lme4::lmer(CHANGE1 ~ GROUPING + BASELINE + (1|SUBJECTNUM), na.action = na.omit))
                  ifelse(!is.null(log.fit.pla2), log.pla2 <- summary(log.fit.pla2), log.pla2$AIC <- NA)
                  log.pla2$AIC
                } else {
                  log.pla2$AIC <- NA
                }
                
                log.aics <- data.frame(log.csxx$AIC, log.cshx$AIC, log.ar1x$AIC, log.arh1$AIC, log.plai$AIC, log.pla2$AIC)
                log.m.name <- names(log.aics)[which.min(apply(log.aics,MARGIN=2,min))]
                log.f.model <- paste0(substr(log.m.name, 1, 4),"fit.", substr(log.m.name, 5, 8))
                #cat("Final Covariance Structure for Log transformation: ", log.f.model, nl)
                if(exists("log.f.model")) cat(silver("  Final Covariance Structure for log-transformed data: " %+% log.f.model), nl)
                
              } else if(design == "crossover"){
                
                #crossover
                if(substr(covs, 1, 1)=="T"){
                  log.fit.csxx <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                               cor=corCompSymm(), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
                  ifelse(!is.null(log.fit.csxx), log.csxx <- summary(log.fit.csxx), log.csxx$AIC <- NA)
                  log.csxx$AIC
                  
                } else {
                  log.csxx$AIC <- NA
                }
                
                if(substr(covs, 2, 2)=="T"){
                  log.fit.cshx <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                               cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
                  #cshx <- summary(log.fit.cshx)
                  ifelse(!is.null(log.fit.cshx), log.cshx <- summary(log.fit.cshx), log.cshx$AIC <- NA)
                  log.cshx$AIC
                } else {
                  log.cshx$AIC <- NA
                }
                
                if(substr(covs, 3, 3)=="T"){
                  log.fit.ar1x <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                               cor=corAR1(), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
                  
                  #ar1x <- summary(log.fit.ar1x)
                  ifelse(!is.null(log.fit.ar1x), log.ar1x <- summary(log.fit.ar1x), log.ar1x$AIC <- NA)
                  log.ar1x$AIC
                } else {
                  log.ar1x$AIC <- NA
                }
                
                if(substr(covs, 4, 4)=="T"){
                  log.fit.arh1 <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                               corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
                  #arh1 <- summary(log.fit.arh1)
                  ifelse(!is.null(log.fit.arh1), log.arh1 <- summary(log.fit.arh1), log.arh1$AIC <- NA)
                  log.arh1$AIC
                } else {
                  log.arh1$AIC <- NA
                }
                if(substr(covs, 5, 5)=="T"){
                  log.fit.plai <- with( w, nlme::lme(CHANGE1 ~ GROUPING + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                               control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
                  ifelse(!is.null(log.fit.plai), log.plai <- summary(log.fit.plai), log.plai$AIC <- NA)
                  log.plai$AIC
                } else {
                  log.plai$AIC <- NA
                }
                if(substr(covs, 6, 6)=="T"){ ##LME4 package
                  log.fit.pla2 <- with( w, lme4::lmer(CHANGE1 ~ GROUPING + BASELINE + SEQ + PERIOD + (1|SUBJECTNUM), na.action = na.omit))
                  ifelse(!is.null(log.fit.pla2), log.pla2 <- summary(log.fit.pla2), log.pla2$AIC <- NA)
                  log.pla2$AIC
                } else {
                  log.pla2$AIC <- NA
                }
                
                log.aics <- data.frame(log.csxx$AIC, log.cshx$AIC, log.ar1x$AIC, log.arh1$AIC, log.plai$AIC, log.pla2$AIC)
                log.m.name <- names(log.aics)[which.min(apply(log.aics,MARGIN=2,min))]
                log.f.model <- paste0(substr(log.m.name, 1, 4),"fit.", substr(log.m.name, 5, 8))
                cat("Final Covariance Structure for Log transformation: ", log.f.model, nl)
                
              }
              
              #check for normality
              lev.test2 <- F
              l.test2 <- NA
              if (log.f.model == "log.fit.csxx"){
                log.final.model <- log.fit.csxx
              } 
              else if(log.f.model == "log.fit.cshx") {
                log.final.model <- log.fit.cshx
              } 
              else if(log.f.model == "log.fit.ar1x") {
                log.final.model <- log.fit.ar1x
              } 
              else if(log.f.model == "log.fit.arh1") {
                log.final.model <- log.fit.arh1
              } 
              else if(log.f.model == "log.fit.plai") {
                log.final.model <- log.fit.plai
                lev.test2 <- T
              } 
              else if(log.f.model == "log.fit.pla2") {
                log.final.model <- log.fit.pla2
                lev.test2 <- T
              } 
              else {
                return("Cant find log model")
              }
              s.test2 <- try(shapiro.test(resid(log.final.model))$p.value)
              if(isTRUE(lev.test2)){
                #assum.m <- lm(CHANGE1 ~ GROUPING, data=w)
                #l.test2 <- try(leveneTest(assum.m)$`Pr(>F)`[1])
              } else {
                #l.test2 <- 1
              }
              #cat("Change Shapiro Test 2: ", s.test2, nl)
              #cat("Change Levene's Test 2: ", l.test2, nl, nl)
              if(exists("s.test2")) cat(silver("  Shapiro Test for non-transformed data: " %+% as.character(s.test2)), nl)
              
              if( s.test2 < 0.01 ){
                #if( s.test2 < 0.91 | l.test2 < 0.05){
                
                if(length(ggroups) == 1){
                  p.val <- NA
                  
                } else if(length(ggroups) == 2){
                  n=1
                  em.btwn.p <- NA
                  #em.pairwise <- data.frame(AB=NA, AC=NA, BC=NA, AD=NA, BD=NA, CD=NA, AE=NA, BE=NA, CE=NA, DE=NA, AF=NA, BF=NA, CF=NA, DF=NA, EF=NA)
                  result <- wilcox.test( CHANGE ~ GROUPING, data = w )
                  #em.btwn.p[n] <- as.numeric(result$p.value)
                  em.btwn.p[n] <- paste0(result$p.value, " (w)")
                  
                  n=n+1
                  
                  #em.btwn <- data.frame( em.btwn.p )
                  #names( em.btwn ) <- "p.value"
                  #cat("#-- Between p-values from Wilcox test", nl)
                  cat(silver("  Final between p-values obtained from Wilcox test"), nl)
                  #print(em.btwn.p) 
                  em.btwn <- data.frame( em.btwn.p )
                  names( em.btwn ) <- "p.value"
                  
                  #EM <- emmeans( final.model, ~GROUPING )
                  #final.model2 <- update( EM, infer = c( TRUE, TRUE ))
                  #em.within <- data.frame( final.model2 )
                  
                  
                  #within.output <- WithinPaired( w.imp, baselinevisit=baselinevisit, speccomp1=speccomp1, speccomp2=speccomp2, i=i, big.p.val, output )
                  #cat("#Within Output:", nl)
                  #print(data.frame(within.output))
                  #for(r in ggroups){
                  #w.p.val <- as.numeric(within.output[1, r])
                  #w.p.val <- within.output[1, r]
                  #em.within[nrow(em.within) + 1,] <- list(r, q, NA, NA, NA, NA, NA, NA, w.p.val)
                  
                  #em.within[em.within$GROUPING == r, "p.value" ] <- w.p.val
                  #}
                  #cat("#-- Paired t-test/Wilcoxon test p-values inserted --#", nl)
                  #print(em.within)
                  
                } else if(length(ggroups) > 2){
                  n=1
                  em.btwn.p <- NA
                  result <- kruskal.test( CHANGE ~ GROUPING, data = w )
                  #em.btwn.p[n] <- as.numeric(result$p.value)
                  em.btwn.p[n] <- paste0(result$p.value, " (k)")
                  
                  if(isTRUE(runpairwise)){
                    #em.pairwise <- data.frame(AB=NA, AC=NA, BC=NA, AD=NA, BD=NA, CD=NA, AE=NA, BE=NA, CE=NA, DE=NA, AF=NA, BF=NA, CF=NA, DF=NA, EF=NA)
                    posthoc <- PMCMRplus::kwAllPairsNemenyiTest(CHANGE ~ GROUPING, data = w, p.adjust.method=NULL)
                    print(posthoc)
                    #em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3],
                    #                posthoc$p.value[4,1], posthoc$p.value[4,2], posthoc$p.value[4,3], posthoc$p.value[4,4],
                    #                posthoc$p.value[5,1], posthoc$p.value[5,2], posthoc$p.value[5,3], posthoc$p.value[5,4], posthoc$p.value[5,5]
                    #)
                    #grpcount <- length(ggroups)
                    #em.pair.p <- NULL
                    #if(grpcount == 6){
                    #  em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                  posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3],
                    #                  posthoc$p.value[4,1], posthoc$p.value[4,2], posthoc$p.value[4,3], posthoc$p.value[4,4],
                    #                  posthoc$p.value[5,1], posthoc$p.value[5,2], posthoc$p.value[5,3], posthoc$p.value[5,4], posthoc$p.value[5,5])
                    #} else if(grpcount == 5){
                    #  em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                  posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3],
                    #                  posthoc$p.value[4,1], posthoc$p.value[4,2], posthoc$p.value[4,3], posthoc$p.value[4,4])
                    #} else if(grpcount == 4){
                    #  em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2], 
                    #                  posthoc$p.value[3,1], posthoc$p.value[3,2], posthoc$p.value[3,3])
                    #} else if(grpcount == 3){
                    #  em.pair.p <- c( posthoc$p.value[1,1], posthoc$p.value[2,1], posthoc$p.value[2,2])
                    #}
                    #print(em.pair.p)
                    #em.pairwise[n,] <- rbind(em.pair.p)
                    
                    #n=n+1
                    
                    posthoc1 <- as.matrix(posthoc$p.value)
                    posthoc2 <- as.data.frame(as.table(posthoc1))
                    posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
                    posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
                    posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
                    em.pairwise <- posthoc3[c("contrast", "p.value")]
                    #print(em.pairwise)
                    
                    if(isTRUE(adj)){
                      em.pairwise.adj <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("contrast", "p.value") )
                      posthoc <- PMCMRplus::kwAllPairsNemenyiTest(CHANGE ~ GROUPING, data = w, p.adjust.method="single-step")
                      print(posthoc)
                      posthoc1 <- as.matrix(posthoc$p.value)
                      posthoc2 <- as.data.frame(as.table(posthoc1))
                      posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
                      posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
                      posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
                      em.pairwise.adj <- posthoc3[c("contrast", "p.value")]
                      #print(em.pairwise.adj)
                    }
                  }
                  #em.btwn <- data.frame( em.btwn.p )
                  #names( em.btwn ) <- "p.value"
                  #cat("#-- Between p-values from Kruskal test", nl)
                  cat(silver("  Final between p-values obtained from Kruskal-Wallis test"), nl)
                  #print(em.btwn.p) 
                  em.btwn <- data.frame( em.btwn.p )
                  names( em.btwn ) <- "p.value"
                  
                  #EM <- emmeans( final.model, ~GROUPING )
                  #final.model2 <- update( EM, infer = c( TRUE, TRUE ))
                  #em.within <- data.frame( final.model2 )
                  
                }
                
                
              } 
              else {
                
                if(design == "parallel"){
                  EM <- emmeans( log.final.model, ~GROUPING )
                } else if(design == "crossover"){
                  EM <- emmeans( log.final.model, ~GROUPING + SEQ + PERIOD )
                }
                final.model2 <- update( EM, infer = c( TRUE, TRUE ))
                #em.within <- data.frame( final.model2 )
                #return(em.within)
                EM4 <- joint_tests( final.model2 )
                em.btwn <- data.frame( EM4 )
                #return(em.btwn)
                ## pairwise comparisons 
                #EM5 <- contrast(final.model2, "consec")
                em.btwn$p.value <- ifelse(is.na(em.btwn$p.value), NA, paste0(em.btwn$p.value, " (l)"))
                if(isTRUE(runpairwise)){
                  EM <- emmeans( log.final.model, ~GROUPING )
                  final.model2 <- update( EM, infer = c( TRUE, TRUE ))
                  EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
                  em.pairwise <- data.frame( EM5 )
                  em.pairwise$p.value <- paste0(n_decimals(em.pairwise$p.value, 3), " (l)")
                  if(isTRUE(adj)){
                    EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
                    em.pairwise.adj <- data.frame( EM6 )
                    em.pairwise.adj$p.value <- paste0(n_decimals(em.pairwise.adj$p.value, 3), " (l)")
                  }
                }
              }
            }
          } else  {
            
            
            if(design == "parallel"){
              EM <- emmeans( final.model, ~GROUPING )
            } else if(design == "crossover"){
              EM <- emmeans( final.model, ~GROUPING + SEQ + PERIOD)
            }
            #return(em.within)
            final.model2 <- update( EM, infer = c( TRUE, TRUE ))
            #em.within <- data.frame( final.model2 )
            #return(em.within)
            EM4 <- joint_tests( final.model2 )
            em.btwn <- data.frame( EM4 )
            #return(em.btwn)
            ## pairwise comparisons 
            #EM5 <- contrast(final.model2, "consec")
            if(isTRUE(runpairwise)){
              EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
              em.pairwise <- data.frame( EM5 )
              em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
              if(isTRUE(adj)){
                EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
                em.pairwise.adj <- data.frame( EM6 )
                em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
              }
            }
          }
          
        }
      }
      
      
      ##### Output things #####
      the.p.val <- as.character(em.btwn$p.value[1])
      #cat("the.p.val", the.p.val, nl)
      if(substring_right(the.p.val, 3) %in% c("(l)", "(r)", "(w)", "(k)")){
        filesuffix <- substring_right(the.p.val, 3)
        a <- substr(the.p.val, 1, 7)
        a <- as.numeric(a)
        p.val <- n_decimals(a, 3)
        if(p.val == "0.000"){ 
          p.val <- "<0.001"
        }
        p.val <- paste0(p.val, " ", filesuffix)
      }  else {
        a <- as.numeric(the.p.val)
        p.val <- n_decimals(a, 3)
        if(p.val == "0.000"){ 
          p.val <- "<0.001"
        }
        p.val <- as.character(p.val)
      }
      
      #p.val <- n_decimals(em.btwn$p.value[l], 3)
      
      #means & SDs
      ms.output.list <- list()
      ms.output.list[["#"]] <- paste0("Change from ", baselinevisit, " to ", i)
      for(j in 1:grps) {
        ms.output.list[[group.names[j]]] <- paste0( n_decimals(chgsumm$x[j,5], 2), " \u00B1 ", n_decimals(chgsumm$x[j,6], 2), " (",chgsumm$x[j,1],")" )
      }
      ms.output.list[["p.val"]] <- p.val
      output[nrow(output) + 1,] = ms.output.list
      
      #medians & ranges
      mr.output.list <- list()
      mr.output.list[["#"]] <- " "
      for(m in 1:grps) {
        mr.output.list[[group.names[m]]] <- paste0( n_decimals(chgsumm$x[m,2], 2), " (", n_decimals(chgsumm$x[m,3], 2), " to ", n_decimals(chgsumm$x[m,4], 2), ")" )
      }
      mr.output.list[["p.val"]] <- " "
      output[nrow(output) + 1,] = mr.output.list
      
      #LSMEANS & SEM
      #return(em.within)
      #Group.A <- paste(n_decimals(em.within$emmean[k], 2), " (",n_decimals(em.within$SE[k], 2),")", sep="")
      #Group.B <- paste(n_decimals(em.within$emmean[k+1], 2), " (",n_decimals(em.within$SE[k+1], 2),")", sep="")
      #output[nrow(output) + 1,] = list(Group.A, Group.B, "")
      
      ## within groups t-test 
      #if(isTRUE(withinpairs)){
      #print("DB from ChangeSums")
      #print(summary(w.imp))
      #output <- WithinPaired(w.imp, baselinevisit, speccomp1, speccomp2, i, big.p.val, normoverride, ggroups, grps, output)
      output <- WithinPaired(w.imp, baselinevisit, speccomp1, speccomp2, i, normoverride, ggroups, grps, output)
      #}
      
      #percent changes
      mysumm <- with( w.noimp[which( w.noimp$VISITNUMBER == baselinevisit ),], aggregate(OUTCOME, by=list(GROUP = GROUPING), FUN = f) )
      percent.output.list <- list()
      for(q in 1:grps) {
        percent.raw <- (chgsumm$x[q,5] / mysumm$x[q,5]) * 100
        percent.output.list[[group.names[q]]] <- paste0(n_decimals(percent.raw, 1), "%")
      }
      output.pchange[nrow(output.pchange) + 1,] = percent.output.list
      #percent.output.list[["p.val"]] <- " "
      #output[nrow(output) + 1,] = percent.output.list
      
      #k=k+6
      #l=l+1
      
      
      
      
      #calculate time elapsed and reset the clock
      toc(); tic();
      cat(nl)
    }
    
    
    #Run pairwise comparisons?
    if(isTRUE(runpairwise)){
      # print(em.pairwise)
      # print(em.pairwise.adj)
      
      
      output.pairwise <- em.pairwise
      
      
      
      if(isTRUE(adj)){
        output.pairwise.adj <- em.pairwise.adj
        
        
      } 
      
    }
    #Export pairwis♥e comparisons
    if(isTRUE(runpairwise)){
      # output.pairwise <- em.pairwise[c("contrast", "VISITNUMBER", "p.value")]
      # if(isTRUE(adj)){
      #   output.pairwise <- em.pairwise.adj[c("contrast", "VISITNUMBER", "p.value")]
      # }
      
      output.pairwise.title <- paste0("Pairwise comparisons for ", outcomevar, " on ", baselinevisit ," and ", toString.default(visitnumbers), "; change in ", outcomevar, " from ", baselinevisit ," to ", toString.default(visitnumbers), " in the ",popn ," population")
      payload[[paste0("CHG TITLE: ", outcomevar, filesuffix, "pairwise_unadj")]] <- output.pairwise.title
      payload[[paste0("CHG ", outcomevar, filesuffix, "pairwise_unadj")]] <- output.pairwise
      
      if(isTRUE(adj)){
        # output.pairwise.adj <- em.pairwise.adj
        payload[[paste0("CHG TITLE (ADJ): ", outcomevar, filesuffix, "pairwise_adj")]] <- paste0("Adjusted,", output.pairwise.title)
        payload[[paste0("CHG (ADJ) ", outcomevar, filesuffix, "pairwise_adj")]] <- output.pairwise.adj
      }
    }
    
    
    #E♣port percent change output
    output.pchange <- q.desc_stats(w.noimp[w.noimp$VISITNUMBER != baselinevisit,], outcomevar="PERCENT", groupvar="GROUPING", timevar="VISITNUMBER")
    
    output.pchange.title <- paste0("Percent changes for ", outcomevar, " from ", baselinevisit ," to ", toString.default(visitnumbers), " in the ",popn ," population")
    payload[[paste0("TITLE: ", outcomevar, filesuffix, "pchange")]] <- output.pchange.title
    payload[[paste0(outcomevar, filesuffix, "pchange")]] <- output.pchange
    
    
    #Export main output
    output <- RenameHeaders(output)
    output.title <- paste0(outcomevar, " on$$at ", baselinevisit ," and ", toString.default(visitnumbers), "; change in ", outcomevar, " from ", baselinevisit ," to ", toString.default(visitnumbers), " in the ",popn ," population")
    payload[[paste0("TITLE: ", outcomevar, filesuffix)]] <- output.title
    payload[[paste0(outcomevar, filesuffix)]] <- output
    cat(green("v ") %+% blue("SUMMARY"), nl)
    # print(payload)
    
    
    
    # result <- list(payload, payload.pairwise, payload)
    
    #Export to file 
    if(".csv" %in% exportfile){
      # utils::write.csv(output, 
      #                  file = paste(exppath, outcomevar, "_", filesuffix, ".csv", sep=""))
      # if(isTRUE(runpairwise)){
      #   utils::write.csv(output.pairwise, 
      #                    file = paste(exppath, outcomevar, "_", filesuffix, 
      #                                 "_Pairwise.csv", sep=""))
      # }
      
      utils::write.csv(output, 
                       file = paste(exppath, outcomevar, filesuffix, ".csv", sep=""))
      if(isTRUE(runpairwise)){
        utils::write.csv(output.pairwise, 
                         file = paste(exppath, outcomevar, filesuffix, 
                                      "_Pairwise.csv", sep=""))
      }
      
      utils::write.csv(output.pchange, 
                       file = paste(exppath, outcomevar, filesuffix, "_PercentChange_", ".csv", sep=""))
    }
    
    
    if(".doc" %in% exportfile){
      q.write.to.word(payload, exportpath=exportpath, 
                      docname=paste0(outcomevar, filesuffix))
      
      # if(isTRUE(runpairwise)){
      #   q.write.to.word(payload.pairwise, exportpath=exportpath, 
      #                   docname=paste0(outcomevar, filesuffix, "_Pairwise_"))
      # }
      # 
      # q.write.to.word(output.pchange, exportpath=exportpath, 
      #                 docname=paste0(outcomevar, filesuffix, "_PercentChange_"))
    }
    
    toc(); cat(nl);
    return(payload)
    
  }
  #'
  #'
  #'
  #'
  #####5. REPEATED MEASURES#####
  ChangeSumsRepeatedCombined <- function(db, db.imp, outcomevar, baselinevisit, visitnumbers, speccomp1, speccomp2, 
                                         filesuffix, runpairwise, adj, within.group, covs, assume.normal.dist, useranks,
                                         ggroups, grps, group.names, output, output.pairwise,  output.pairwise.adj, design ){
    tic()
    w.noimp <- db
    w.noimp <- db.imp #comment this out to generate descriptives using non-imputed dataset 
    w.imp <- db.imp

    #cat( nl, nl, "# ------- CHANGE REPEATED MEASURES -------- #", nl)
    #cat(green("v ") %+% blue("CHANGE REPEATED MEASURES " %+% as.character(i)), nl)
    cat(green("v ") %+% blue("CHANGE REPEATED MEASURES "), nl)
    cat(blue("Time Points: "), silver(as.character(baselinevisit) %+% as.character(toString.default(visitnumbers))), nl)
    
    if(!is.na(speccomp1) & !is.na(speccomp2)){
      visitnumbers = c( visitnumbers, paste0(speccomp1, speccomp2))
    }
    #print(visitnumbers)
    #w <- db[ which( db$LBTESTH == testname),]
    
    #prepare output object and other stuff
    # big.p.val <- list("","","")
    output.pchange <- setNames(data.frame(matrix(ncol = grps, nrow = 0)), c(group.names) )
    em.within <- data.frame(GROUPING = numeric(), VISITNUMBER = numeric(), emmean = numeric(), SE = numeric(), 
                            df = numeric(), lower.CL = numeric(), upper.CL = numeric(), t.ratio = numeric(),
                            p.value = character(), stringsAsFactors = FALSE)
    
    ##### Model things #####
    ## Check covariance matrices ##
    # w <- w.imp[!is.na(w.imp$CHANGE), ] #use only if dependent var in model is CHANGE
    # w <- w[w$VISITNUMBER != baselinevisit, ] #use only if dependent var in model is CHANGE
    
    w <- w.imp #use only if dependent var in model is OUTCOME
    #print(summary(w))
    
    if(grps == 1){ #+ single arm study####
      
      csxx <- cshx <- ar1x <- arh1 <- plai <- pla2 <- pla3 <- list()
      
      if(substr(covs, 1, 1)=="T"){
        fit.csxx <- with( w, nlme::lme(OUTCOME ~ VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                 cor=corCompSymm(), 
                                 control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
        ifelse(!is.null(fit.csxx), csxx <- summary(fit.csxx), csxx$AIC <- NA)
        csxx$AIC
      } else {
        csxx$AIC <- NA
      }
      
      if(substr(covs, 2, 2)=="T"){
        fit.cshx <- with( w, nlme::lme(OUTCOME ~ VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                 cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                 control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
        #cshx <- summary(fit.cshx)
        ifelse(!is.null(fit.cshx), cshx <- summary(fit.cshx), cshx$AIC <- NA)
        cshx$AIC
      } else {
        cshx$AIC <- NA
      }
      
      if(substr(covs, 3, 3)=="T"){
        fit.ar1x <- with( w, nlme::lme(OUTCOME ~ VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                 cor=corAR1(), 
                                 control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
        
        #ar1x <- summary(fit.ar1x)
        ifelse(!is.null(fit.ar1x), ar1x <- summary(fit.ar1x), ar1x$AIC <- NA)
        ar1x$AIC
      } else {
        ar1x$AIC <- NA
      }
      
      if(substr(covs, 4, 4)=="T"){
        fit.arh1 <- with( w, nlme::lme(OUTCOME ~ VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                 corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                 control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
        #arh1 <- summary(fit.arh1)
        ifelse(!is.null(fit.arh1), arh1 <- summary(fit.arh1), arh1$AIC <- NA)
        arh1$AIC
      } else {
        arh1$AIC <- NA
      }
      if(substr(covs, 5, 5)=="T"){ #PLAIN
        fit.plai <- with( w, nlme::lme(OUTCOME ~ VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                 control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
        ifelse(!is.null(fit.plai), plai <- summary(fit.plai), plai$AIC <- NA)
        plai$AIC
      } else {
        plai$AIC <- NA
      }
      if(substr(covs, 6, 6)=="T"){ ##LME4 package
        fit.pla2 <- with( w, lme4::lmer(OUTCOME ~ VISITNUMBER + BASELINE + (1|SUBJECTNUM), na.action = na.omit))
        ifelse(!is.null(fit.pla2), pla2 <- summary(fit.pla2), pla2$AIC <- NA)
        pla2$AIC
      } else {
        pla2$AIC <- NA
      }
      
      
      
      aics <- data.frame(csxx$AIC, cshx$AIC, ar1x$AIC, arh1$AIC, plai$AIC, pla2$AIC)
      m.name <- names(aics)[which.min(apply(aics,MARGIN=2,min))]
      f.model <- paste0("fit.", substr(m.name, 1, 4))
      cat("Final Covariance Structure: ", f.model, nl)
      
      final.model <- NA
      if (f.model == "fit.csxx"){
        final.model <- fit.csxx
      } 
      else if(f.model == "fit.cshx") {
        final.model <- fit.cshx
      } 
      else if(f.model == "fit.ar1x") {
        final.model <- fit.ar1x
      } 
      else if(f.model == "fit.arh1") {
        final.model <- fit.arh1
      } 
      else if(f.model == "fit.plai") {
        final.model <- fit.plai
        #lev.test <- T
      } else if(f.model == "fit.pla2") {
        final.model <- fit.pla2
        #lev.test <- T
      } 
      else {
        return("Cant find model")
      }
      
      
      if(isTRUE(assume.normal.dist)){
        
        EM <- emmeans( final.model, ~VISITNUMBER)
        final.model2 <- update( EM, infer = c( TRUE, TRUE ))
        # EM4 <- joint_tests( final.model2, by = "VISITNUMBER")
        # em.btwn <- data.frame( EM4 )
        # cat("#-- Between visit p-values from assume.normal.dist parametric test", nl)
        # print(em.btwn)
        ##pairwise comparisons 
        EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
        em.within <- as.data.frame( EM5 )
        cat("#-- Between visit (within) p-values from assume.normal.dist parametric test", nl)
        # print(em.within)
        visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
        visit_pairs1 <- as.data.frame(visit_pairs)
        em.within <- dplyr::bind_cols(em.within, visit_pairs1)
        print(em.within)
        em.within$p.value <- n_decimals(em.within$p.value, 3)
        #adj
        EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
        em.within.adj <- data.frame( EM6 )
        visit_pairs <- NULL
        visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
        visit_pairs1 <- as.data.frame(visit_pairs)
        em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
        em.within.adj$p.value <- n_decimals(em.within.adj$p.value, 3)
        
        
      } else {
        s.test <- shapiro.test(resid(final.model))$p.value
        assumptions.mixed(final.model)
        #l.test <- try(leveneTest(assum.m)$`Pr(>F)`[1])
        cat("Change Shapiro Test 1: ", s.test, nl, nl)
        if( s.test < 0.01 ){
          
          if(isTRUE(useranks)){
            w$OUTCOME2 <- rank(w$OUTCOME, ties.method="max")
            
            rank.m <- lm(OUTCOME2 ~ VISITNUMBER + BASELINE, data = w, na.action = na.omit )
            EM <- emmeans( rank.m, ~VISITNUMBER )
            
            #result <- Anova(rank.m)
            #p.val <- n_decimals(result$`Pr(>F)`[1], 3)
            #p.val <- paste0(p.val, " (r)")
            
            rank.m2 <- update( EM, infer = c( TRUE, TRUE ))
            # EM4 <- joint_tests( rank.m2, by = "GROUPING" )
            # em.btwn <- data.frame( EM4 )
            # print("Ranked ANOVA: ")
            # print(em.btwn)
            # em.btwn$p.value <- ifelse(is.na(em.btwn$p.value), NA, paste0(em.btwn$p.value, " (r)"))
            EM5 <- contrast( rank.m2, "pairwise", adjust = NULL )
            em.within <- as.data.frame( EM5 )
            cat("#-- Between visit (within) p-values from ranked test", nl)
            # print(em.within)
            visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
            visit_pairs1 <- as.data.frame(visit_pairs)
            em.within <- dplyr::bind_cols(em.within, visit_pairs1)
            print(em.within)
            # em.within$p.value <- n_decimals(em.within$p.value, 3)
            em.within$p.value <- paste0(n_decimals(em.within$p.value, 3), " (r)")
            EM6 <- contrast( rank.m2, "pairwise", adjust = "tukey" )
            #adj
            em.within.adj <- data.frame( EM6 )
            visit_pairs <- NULL
            visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
            visit_pairs1 <- as.data.frame(visit_pairs)
            em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
            # em.within.adj$p.value <- n_decimals(em.within.adj$p.value, 3)
            em.within.adj$p.value <- paste0(n_decimals(em.within.adj$p.value, 3), " (r)")
            
            
          } else{
            w$OUTCOME1 <- log(w$OUTCOME + (1 - min(w$OUTCOME, na.rm = TRUE)))
            
            cat("OUTCOME1 Length: ", length(w$OUTCOME1), nl, nl)
            log.csxx <- list()
            log.cshx <- list()
            log.ar1x <- list()
            log.arh1 <- list()
            log.plai <- list()
            log.pla2 <- list()
            
            
            if(substr(covs, 1, 1)=="T"){
              log.fit.csxx <- with( w, nlme::lme(OUTCOME1 ~ VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                           cor=corCompSymm(), 
                                           control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
              ifelse(!is.null(log.fit.csxx), log.csxx <- summary(log.fit.csxx), log.csxx$AIC <- NA)
              log.csxx$AIC
              
            } else {
              log.csxx$AIC <- NA
            }
            
            if(substr(covs, 2, 2)=="T"){
              log.fit.cshx <- with( w, nlme::lme(OUTCOME1 ~ VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                           cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                           control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
              #cshx <- summary(log.fit.cshx)
              ifelse(!is.null(log.fit.cshx), log.cshx <- summary(log.fit.cshx), log.cshx$AIC <- NA)
              log.cshx$AIC
            } else {
              log.cshx$AIC <- NA
            }
            
            if(substr(covs, 3, 3)=="T"){
              log.fit.ar1x <- with( w, nlme::lme(OUTCOME1 ~ VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                           cor=corAR1(), 
                                           control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
              
              #ar1x <- summary(log.fit.ar1x)
              ifelse(!is.null(log.fit.ar1x), log.ar1x <- summary(log.fit.ar1x), log.ar1x$AIC <- NA)
              log.ar1x$AIC
            } else {
              log.ar1x$AIC <- NA
            }
            
            if(substr(covs, 4, 4)=="T"){
              log.fit.arh1 <- with( w, nlme::lme(OUTCOME1 ~ VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                           corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                           control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
              #arh1 <- summary(log.fit.arh1)
              ifelse(!is.null(log.fit.arh1), log.arh1 <- summary(log.fit.arh1), log.arh1$AIC <- NA)
              log.arh1$AIC
            } else {
              log.arh1$AIC <- NA
            }
            if(substr(covs, 5, 5)=="T"){
              log.fit.plai <- with( w, nlme::lme(OUTCOME1 ~ VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                           control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
              ifelse(!is.null(log.fit.plai), log.plai <- summary(log.fit.plai), log.plai$AIC <- NA)
              log.plai$AIC
            } else {
              log.plai$AIC <- NA
            }
            if(substr(covs, 6, 6)=="T"){ ##LME4 package
              log.fit.pla2 <- with( w, lme4::lmer(OUTCOME1 ~ VISITNUMBER + BASELINE + (1|SUBJECTNUM), na.action = na.omit))
              ifelse(!is.null(log.fit.pla2), log.pla2 <- summary(log.fit.pla2), log.pla2$AIC <- NA)
              log.pla2$AIC
            } else {
              log.pla2$AIC <- NA
            }
            
            log.aics <- data.frame(log.csxx$AIC, log.cshx$AIC, log.ar1x$AIC, log.arh1$AIC, log.plai$AIC, log.pla2$AIC)
            log.m.name <- names(log.aics)[which.min(apply(log.aics,MARGIN=2,min))]
            log.f.model <- paste0(substr(log.m.name, 1, 4),"fit.", substr(log.m.name, 5, 8))
            cat("Final Covariance Structure for Log transformation: ", log.f.model, nl)
            
            
            
            
            #check for normality
            lev.test2 <- F
            l.test2 <- NA
            if (log.f.model == "log.fit.csxx"){
              log.final.model <- log.fit.csxx
            } 
            else if(log.f.model == "log.fit.cshx") {
              log.final.model <- log.fit.cshx
            } 
            else if(log.f.model == "log.fit.ar1x") {
              log.final.model <- log.fit.ar1x
            } 
            else if(log.f.model == "log.fit.arh1") {
              log.final.model <- log.fit.arh1
            } 
            else if(log.f.model == "log.fit.plai") {
              log.final.model <- log.fit.plai
              lev.test2 <- T
            } else if(log.f.model == "log.fit.pla2") {
              log.final.model <- log.fit.pla2
              lev.test2 <- T
            }
            else {
              return("Cant find log model")
            }
            s.test2 <- try(shapiro.test(resid(log.final.model))$p.value)
            if(isTRUE(lev.test2)){
              assum.m <- lm(OUTCOME1 ~ VISITNUMBER, data=w)
              l.test2 <- try(leveneTest(assum.m)$`Pr(>F)`[1])
            } else {
              l.test2 <- 1
            }
            cat("Change Shapiro Test 2: ", s.test2, nl)
            cat("Change Levene's Test 2: ", l.test2, nl, nl)
            
            if( s.test2 < 0.01 ){
              
              if(length(ggroups) == 1){
                p.val <- NA
                
              } else if(length(ggroups) == 2){
                n=1
                em.btwn.p <- NA
                #em.pairwise <- data.frame(AB=NA, AC=NA, BC=NA, AD=NA, BD=NA, CD=NA, AE=NA, BE=NA, CE=NA, DE=NA, AF=NA, BF=NA, CF=NA, DF=NA, EF=NA)
                for( m in visitnumbers ){
                  result <- wilcox.test( OUTCOME ~ VISITNUMBER, data = w[w$VISITNUMBER == m, ] )
                  em.btwn.p[n] <- paste0(sprintf("%.5f", result$p.value), " (w)")
                  
                  
                  n=n+1
                }
                cat("#-- Between p-values from Wilcox test", nl)
                print(em.btwn.p) 
                em.btwn <- data.frame( em.btwn.p )
                names( em.btwn ) <- "p.value"
                
                
              } 
              
              
            } 
            else {
              EM <- emmeans( log.final.model, ~VISITNUMBER )
              
              final.model2 <- update( EM, infer = c( TRUE, TRUE ))
              #em.within <- data.frame( final.model2 )
              #return(em.within)
              #print(em.within)
              # EM4 <- joint_tests( final.model2, by = "VISITNUMBER" )
              # em.btwn <- data.frame( EM4 )
              # cat("#-- Between p-values from Log transform", nl)
              # print(em.btwn)
              ##pairwise comparisons 
              em.btwn$p.value <- ifelse(is.na(em.btwn$p.value), NA, paste0(sprintf("%.5f", em.btwn$p.value), " (l)"))
              EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
              em.within <- as.data.frame( EM5 )
              cat("#-- Between visit (within) p-values from assume.normal.dist parametric test", nl)
              # print(em.within)
              visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
              visit_pairs1 <- as.data.frame(visit_pairs)
              em.within <- dplyr::bind_cols(em.within, visit_pairs1)
              print(em.within)
              em.within$p.value <- paste0(n_decimals(em.within$p.value, 3), " (l)")
              #adj
              EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
              em.within.adj <- data.frame( EM6 )
              visit_pairs <- NULL
              visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
              visit_pairs1 <- as.data.frame(visit_pairs)
              em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
              # em.within.adj$p.value <- n_decimals(em.within.adj$p.value, 3)
              em.within.adj$p.value <- paste0(n_decimals(em.within.adj$p.value, 3), " (l)")
            }
          }
        } else  {
          
          EM <- emmeans( final.model, ~VISITNUMBER)
          
          final.model2 <- update( EM, infer = c( TRUE, TRUE ))
          # EM4 <- joint_tests( final.model2, by = "VISITNUMBER")
          # em.btwn <- data.frame( EM4 )
          # cat("#-- Between visit p-values from assume.normal.dist parametric test", nl)
          # print(em.btwn)
          ##pairwise comparisons 
          EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
          em.within <- as.data.frame( EM5 )
          cat("#-- Between visit (within) p-values from assume.normal.dist parametric test", nl)
          # print(em.within)
          visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
          visit_pairs1 <- as.data.frame(visit_pairs)
          em.within <- dplyr::bind_cols(em.within, visit_pairs1)
          print(em.within)
          em.within$p.value <- n_decimals(em.within$p.value, 3)
          #Adj
          EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
          em.within.adj <- data.frame( EM6 )
          visit_pairs <- NULL
          visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
          visit_pairs1 <- as.data.frame(visit_pairs)
          em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
          em.within.adj$p.value <- n_decimals(em.within.adj$p.value, 3)
        }
        
      } 
      
      
      
      
    } else if(grps > 1){  #+multi arm study ######
      
      csxx <- cshx <- ar1x <- arh1 <- plai <- pla2 <- pla3 <- list()
      
      
      if(design == "parallel"){
        if(substr(covs, 1, 1)=="T"){
          fit.csxx <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                   cor=corCompSymm(), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
          ifelse(!is.null(fit.csxx), csxx <- summary(fit.csxx), csxx$AIC <- NA)
          csxx$AIC
        } else {
          csxx$AIC <- NA
        }
        
        if(substr(covs, 2, 2)=="T"){
          fit.cshx <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                   cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
          #cshx <- summary(fit.cshx)
          ifelse(!is.null(fit.cshx), cshx <- summary(fit.cshx), cshx$AIC <- NA)
          cshx$AIC
        } else {
          cshx$AIC <- NA
        }
        
        if(substr(covs, 3, 3)=="T"){
          fit.ar1x <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                   cor=corAR1(), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
          
          #ar1x <- summary(fit.ar1x)
          ifelse(!is.null(fit.ar1x), ar1x <- summary(fit.ar1x), ar1x$AIC <- NA)
          ar1x$AIC
        } else {
          ar1x$AIC <- NA
        }
        
        if(substr(covs, 4, 4)=="T"){
          fit.arh1 <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                   corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
          #arh1 <- summary(fit.arh1)
          ifelse(!is.null(fit.arh1), arh1 <- summary(fit.arh1), arh1$AIC <- NA)
          arh1$AIC
        } else {
          arh1$AIC <- NA
        }
        if(substr(covs, 5, 5)=="T"){ #PLAIN
          fit.plai <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
          ifelse(!is.null(fit.plai), plai <- summary(fit.plai), plai$AIC <- NA)
          plai$AIC
        } else {
          plai$AIC <- NA
        }
        if(substr(covs, 6, 6)=="T"){ ##LME4 package
          fit.pla2 <- with( w, lme4::lmer(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE + (1|SUBJECTNUM), na.action = na.omit))
          ifelse(!is.null(fit.pla2), pla2 <- summary(fit.pla2), pla2$AIC <- NA)
          pla2$AIC
        } else {
          pla2$AIC <- NA
        }
        
        
        
        aics <- data.frame(csxx$AIC, cshx$AIC, ar1x$AIC, arh1$AIC, plai$AIC, pla2$AIC)
        m.name <- names(aics)[which.min(apply(aics,MARGIN=2,min))]
        f.model <- paste0("fit.", substr(m.name, 1, 4))
        cat("Final Covariance Structure: ", f.model, nl)
        
      } else if(design == "crossover"){
        #Crossover
        if(substr(covs, 1, 1)=="T"){
          fit.csxx <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                   cor=corCompSymm(), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
          ifelse(!is.null(fit.csxx), csxx <- summary(fit.csxx), csxx$AIC <- NA)
          csxx$AIC
        } else {
          csxx$AIC <- NA
        }
        
        if(substr(covs, 2, 2)=="T"){
          fit.cshx <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                   cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
          #cshx <- summary(fit.cshx)
          ifelse(!is.null(fit.cshx), cshx <- summary(fit.cshx), cshx$AIC <- NA)
          cshx$AIC
        } else {
          cshx$AIC <- NA
        }
        
        if(substr(covs, 3, 3)=="T"){
          fit.ar1x <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                   cor=corAR1(), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
          
          #ar1x <- summary(fit.ar1x)
          ifelse(!is.null(fit.ar1x), ar1x <- summary(fit.ar1x), ar1x$AIC <- NA)
          ar1x$AIC
        } else {
          ar1x$AIC <- NA
        }
        
        if(substr(covs, 4, 4)=="T"){
          fit.arh1 <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                   corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
          #arh1 <- summary(fit.arh1)
          ifelse(!is.null(fit.arh1), arh1 <- summary(fit.arh1), arh1$AIC <- NA)
          arh1$AIC
        } else {
          arh1$AIC <- NA
        }
        if(substr(covs, 5, 5)=="T"){
          fit.plai <- with( w, nlme::lme(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                   control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
          ifelse(!is.null(fit.plai), plai <- summary(fit.plai), plai$AIC <- NA)
          plai$AIC
        } else {
          plai$AIC <- NA
        }
        if(substr(covs, 6, 6)=="T"){ ##LME4 package
          fit.pla2 <- with( w, lme4::lmer(OUTCOME ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD + (1|SUBJECTNUM), na.action = na.omit))
          ifelse(!is.null(fit.pla2), pla2 <- summary(fit.pla2), pla2$AIC <- NA)
          pla2$AIC
        } else {
          pla2$AIC <- NA
        }
        
        
        aics <- data.frame(csxx$AIC, cshx$AIC, ar1x$AIC, arh1$AIC, plai$AIC, pla2$AIC)
        m.name <- names(aics)[which.min(apply(aics,MARGIN=2,min))]
        f.model <- paste0("fit.", substr(m.name, 1, 4))
        cat("Final Covariance Structure: ", f.model, nl)
        
        
      }
      
      
      
      
      #check for normality
      lev.test <- F #perform Levene's test?
      l.test <- NA
      final.model <- NA
      if (f.model == "fit.csxx"){
        final.model <- fit.csxx
      } 
      else if(f.model == "fit.cshx") {
        final.model <- fit.cshx
      } 
      else if(f.model == "fit.ar1x") {
        final.model <- fit.ar1x
      } 
      else if(f.model == "fit.arh1") {
        final.model <- fit.arh1
      } 
      else if(f.model == "fit.plai") {
        final.model <- fit.plai
        #lev.test <- T
      } else if(f.model == "fit.pla2") {
        final.model <- fit.pla2
        #lev.test <- T
      } 
      else {
        return("Cant find model")
      }
      
      
      
      
      if(isTRUE(assume.normal.dist)){
        if(design == "parallel"){
          EM <- emmeans( final.model, ~GROUPING*VISITNUMBER)
          
        } else if(design == "crossover"){
          EM <- emmeans( final.model, ~GROUPING*VISITNUMBER + SEQ + PERIOD)
          #em.btwn <- summary(final.model)
        }
        final.model2 <- update( EM, infer = c( TRUE, TRUE ))
        sum.mod <- summary(final.model)
        if(f.model == "fit.pla2"){
          EM4 <- joint_tests( final.model2, by = "VISITNUMBER" )
          em.btwn <- data.frame( EM4 )
          rownames(em.btwn) <- paste0("VISITNUMBER", em.btwn$VISITNUMBER)
          colnames(em.btwn)[colnames(em.btwn) == "p.value"] <- "p-value"
          em.btwn$`p-value` <- sprintf("%.5f", em.btwn$`p-value`)
          cat("#-- Between p-values from Parametric Test:", nl)
          print(em.btwn)
        } else {
          em.btwn <- as.data.frame( sum.mod$tTable )
          em.btwn$`p-value` <- sprintf("%.5f", em.btwn$`p-value`)
          cat("#-- Between p-values from assume.normal.dist parametric test", nl)
          print(em.btwn)
        }
        
        
        ##pairwise comparisons 
        EM5 <- contrast( final.model2, "pairwise", by = "VISITNUMBER", adjust = NULL )
        em.pairwise <- data.frame( EM5 )
        em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
        cat("#-- em.pairwise unadj from assume.normal.dist parametric test", nl)
        #print(em.pairwise)
        EM6 <- contrast( final.model2, "pairwise", by="VISITNUMBER", adjust = "tukey" )
        em.pairwise.adj <- data.frame( EM6 )
        em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
        
        #within group p-values from the model
        EMW <- emmeans( final.model, ~GROUPING*VISITNUMBER)
        final.model2W <- update( EMW, infer = c( TRUE, TRUE ))
        ##pairwise comparisons 
        EMW5 <- contrast( final.model2W, "pairwise", adjust = NULL )
        em.within <- as.data.frame( EMW5 )
        cat("#-- Between visit (within) p-values from assume.normal.dist parametric test", nl)
        # print(em.within)
        visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
        visit_pairs1 <- as.data.frame(visit_pairs)
        em.within <- dplyr::bind_cols(em.within, visit_pairs1)
        print(em.within)
        em.within$p.value <- n_decimals(em.within$p.value, 3)
        #adj
        EMW6 <- contrast( final.model2W, "pairwise", adjust = "tukey" )
        em.within.adj <- data.frame( EMW6 )
        visit_pairs <- NULL
        visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
        visit_pairs1 <- as.data.frame(visit_pairs)
        em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
        em.within.adj$p.value <- n_decimals(em.within.adj$p.value, 3)
        
        
      } else {
        s.test <- shapiro.test(resid(final.model))$p.value
        assumptions.mixed(final.model)
        #l.test <- try(leveneTest(assum.m)$`Pr(>F)`[1])
        cat("Change Shapiro Test 1: ", s.test, nl, nl)
        if( s.test < 0.01 ){
          
          if(isTRUE(useranks)){
            w$OUTCOME2 <- rank(w$OUTCOME, ties.method="max")
            
            if(design == "parallel"){
              rank.m <- lm(OUTCOME2 ~ GROUPING*VISITNUMBER + BASELINE, data = w, na.action = na.omit )
              EM <- emmeans( rank.m, ~GROUPING*VISITNUMBER )
            } else if(design == "crossover"){
              rank.m <- lm(OUTCOME2 ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, data = w, na.action = na.omit )
              EM <- emmeans( rank.m, ~GROUPING*VISITNUMBER + SEQ + PERIOD)
            }
            #result <- Anova(rank.m)
            #p.val <- n_decimals(result$`Pr(>F)`[1], 3)
            #p.val <- paste0(p.val, " (r)")
            
            rank.m2 <- update( EM, infer = c( TRUE, TRUE ))
            sum.mod <- summary(rank.m)
            #print(sum.mod)
            em.btwn <- as.data.frame( sum.mod$coefficients )
            names(em.btwn)[4] <- "p-value"
            em.btwn$`p-value` <- sprintf("%.5f", em.btwn$`p-value`)
            cat("#-- Ranked ANOVA: ", nl)
            print(em.btwn)
            em.btwn$`p-value` <- ifelse(is.na(em.btwn$`p-value`), NA, paste0(em.btwn$`p-value`, " (r)"))
            
            #pairwise
            EM5 <- contrast( rank.m2, "pairwise", by="VISITNUMBER", adjust = NULL )
            em.pairwise <- data.frame( EM5 )
            em.pairwise$p.value <- paste0(n_decimals(em.pairwise$p.value, 3), " (r)")
            EM6 <- contrast( rank.m2, "pairwise", by="VISITNUMBER", adjust = "tukey" )
            em.pairwise.adj <- data.frame( EM6 )
            em.pairwise.adj$p.value <- paste0(n_decimals(em.pairwise.adj$p.value, 3), " (r)")
            
            #within group p-values from the model
            EMW <- emmeans( rank.m, ~GROUPING*VISITNUMBER)
            final.model2W <- update( EMW, infer = c( TRUE, TRUE ))
            ##pairwise comparisons 
            EMW5 <- contrast( final.model2W, "pairwise", adjust = NULL )
            em.within <- as.data.frame( EMW5 )
            cat("#-- Within-group (between visit)  from Ranked test", nl)
            # print(em.within)
            visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
            visit_pairs1 <- as.data.frame(visit_pairs)
            em.within <- dplyr::bind_cols(em.within, visit_pairs1)
            print(em.within)
            em.within$p.value <- paste0(n_decimals(em.within$p.value, 3), " (r)")
            #adj
            EMW6 <- contrast( final.model2W, "pairwise", adjust = "tukey" )
            em.within.adj <- data.frame( EMW6 )
            visit_pairs <- NULL
            visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
            visit_pairs1 <- as.data.frame(visit_pairs)
            em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
            em.within.adj$p.value <- paste0(n_decimals(em.within.adj$p.value, 3), " (r)")
            
          } else{
            w$OUTCOME1 <- log(w$OUTCOME + (1 - min(w$OUTCOME, na.rm = TRUE)))
            
            cat("OUTCOME1 Length: ", length(w$OUTCOME1), nl, nl)
            log.csxx <- list()
            log.cshx <- list()
            log.ar1x <- list()
            log.arh1 <- list()
            log.plai <- list()
            log.pla2 <- list()
            
            if(design == "parallel"){
              if(substr(covs, 1, 1)=="T"){
                log.fit.csxx <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                             cor=corCompSymm(), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
                ifelse(!is.null(log.fit.csxx), log.csxx <- summary(log.fit.csxx), log.csxx$AIC <- NA)
                log.csxx$AIC
                
              } else {
                log.csxx$AIC <- NA
              }
              
              if(substr(covs, 2, 2)=="T"){
                log.fit.cshx <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                             cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
                #cshx <- summary(log.fit.cshx)
                ifelse(!is.null(log.fit.cshx), log.cshx <- summary(log.fit.cshx), log.cshx$AIC <- NA)
                log.cshx$AIC
              } else {
                log.cshx$AIC <- NA
              }
              
              if(substr(covs, 3, 3)=="T"){
                log.fit.ar1x <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE, random= ~1|SUBJECTNUM, 
                                             cor=corAR1(), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
                
                #ar1x <- summary(log.fit.ar1x)
                ifelse(!is.null(log.fit.ar1x), log.ar1x <- summary(log.fit.ar1x), log.ar1x$AIC <- NA)
                log.ar1x$AIC
              } else {
                log.ar1x$AIC <- NA
              }
              
              if(substr(covs, 4, 4)=="T"){
                log.fit.arh1 <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                             corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
                #arh1 <- summary(log.fit.arh1)
                ifelse(!is.null(log.fit.arh1), log.arh1 <- summary(log.fit.arh1), log.arh1$AIC <- NA)
                log.arh1$AIC
              } else {
                log.arh1$AIC <- NA
              }
              if(substr(covs, 5, 5)=="T"){
                log.fit.plai <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE, random=~1|SUBJECTNUM, 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
                ifelse(!is.null(log.fit.plai), log.plai <- summary(log.fit.plai), log.plai$AIC <- NA)
                log.plai$AIC
              } else {
                log.plai$AIC <- NA
              }
              if(substr(covs, 6, 6)=="T"){ ##LME4 package
                log.fit.pla2 <- with( w, lme4::lmer(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE + (1|SUBJECTNUM), na.action = na.omit))
                ifelse(!is.null(log.fit.pla2), log.pla2 <- summary(log.fit.pla2), log.pla2$AIC <- NA)
                log.pla2$AIC
              } else {
                log.pla2$AIC <- NA
              }
              
              log.aics <- data.frame(log.csxx$AIC, log.cshx$AIC, log.ar1x$AIC, log.arh1$AIC, log.plai$AIC, log.pla2$AIC)
              log.m.name <- names(log.aics)[which.min(apply(log.aics,MARGIN=2,min))]
              log.f.model <- paste0(substr(log.m.name, 1, 4),"fit.", substr(log.m.name, 5, 8))
              cat("Final Covariance Structure for Log transformation: ", log.f.model, nl)
              
            } else if(design == "crossover"){
              if(substr(covs, 1, 1)=="T"){
                log.fit.csxx <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                             cor=corCompSymm(), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Compound symmetry
                ifelse(!is.null(log.fit.csxx), log.csxx <- summary(log.fit.csxx), log.csxx$AIC <- NA)
                log.csxx$AIC
                
              } else {
                log.csxx$AIC <- NA
              }
              
              if(substr(covs, 2, 2)=="T"){
                log.fit.cshx <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                             cor=corCompSymm(), weights = varIdent(form = ~1|GROUPING), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120),  na.action = na.omit))#Heterogenous Compound symmetry
                #cshx <- summary(log.fit.cshx)
                ifelse(!is.null(log.fit.cshx), log.cshx <- summary(log.fit.cshx), log.cshx$AIC <- NA)
                log.cshx$AIC
              } else {
                log.cshx$AIC <- NA
              }
              
              if(substr(covs, 3, 3)=="T"){
                log.fit.ar1x <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random= ~1|SUBJECTNUM, 
                                             cor=corAR1(), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Autoregressive
                
                #ar1x <- summary(log.fit.ar1x)
                ifelse(!is.null(log.fit.ar1x), log.ar1x <- summary(log.fit.ar1x), log.ar1x$AIC <- NA)
                log.ar1x$AIC
              } else {
                log.ar1x$AIC <- NA
              }
              
              if(substr(covs, 4, 4)=="T"){
                log.fit.arh1 <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                             corr = corAR1(), weight = varIdent(form = ~ 1|GROUPING), 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#Heterogenous Autoregressive
                #arh1 <- summary(log.fit.arh1)
                ifelse(!is.null(log.fit.arh1), log.arh1 <- summary(log.fit.arh1), log.arh1$AIC <- NA)
                log.arh1$AIC
              } else {
                log.arh1$AIC <- NA
              }
              if(substr(covs, 5, 5)=="T"){
                log.fit.plai <- with( w, nlme::lme(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD, random=~1|SUBJECTNUM, 
                                             control=nlme::lmeControl(singular.ok=TRUE, returnObject=TRUE, msMaxIter=120), na.action = na.omit))#No covariance structure
                ifelse(!is.null(log.fit.plai), log.plai <- summary(log.fit.plai), log.plai$AIC <- NA)
                log.plai$AIC
              } else {
                log.plai$AIC <- NA
              }
              if(substr(covs, 6, 6)=="T"){ ##LME4 package
                log.fit.pla2 <- with( w, lme4::lmer(OUTCOME1 ~ GROUPING*VISITNUMBER + BASELINE + SEQ + PERIOD + (1|SUBJECTNUM), na.action = na.omit))
                ifelse(!is.null(log.fit.pla2), log.pla2 <- summary(log.fit.pla2), log.pla2$AIC <- NA)
                log.pla2$AIC
              } else {
                log.pla2$AIC <- NA
              }
              
              log.aics <- data.frame(log.csxx$AIC, log.cshx$AIC, log.ar1x$AIC, log.arh1$AIC, log.plai$AIC, log.pla2$AIC)
              log.m.name <- names(log.aics)[which.min(apply(log.aics,MARGIN=2,min))]
              log.f.model <- paste0(substr(log.m.name, 1, 4),"fit.", substr(log.m.name, 5, 8))
              cat("Final Covariance Structure for Log transformation: ", log.f.model, nl)
              
            }
            
            
            #check for normality
            lev.test2 <- F
            l.test2 <- NA
            if (log.f.model == "log.fit.csxx"){
              log.final.model <- log.fit.csxx
            } 
            else if(log.f.model == "log.fit.cshx") {
              log.final.model <- log.fit.cshx
            } 
            else if(log.f.model == "log.fit.ar1x") {
              log.final.model <- log.fit.ar1x
            } 
            else if(log.f.model == "log.fit.arh1") {
              log.final.model <- log.fit.arh1
            } 
            else if(log.f.model == "log.fit.plai") {
              log.final.model <- log.fit.plai
              lev.test2 <- T
            } else if(log.f.model == "log.fit.pla2") {
              log.final.model <- log.fit.pla2
              lev.test2 <- T
            }
            else {
              return("Cant find log model")
            }
            s.test2 <- try(shapiro.test(resid(log.final.model))$p.value)
            if(isTRUE(lev.test2)){
              assum.m <- lm(OUTCOME1 ~ GROUPING, data=w)
              l.test2 <- try(leveneTest(assum.m)$`Pr(>F)`[1])
            } else {
              l.test2 <- 1
            }
            cat("Change Shapiro Test 2: ", s.test2, nl)
            cat("Change Levene's Test 2: ", l.test2, nl, nl)
            
            # if( s.test2 < 0.01 ){
            if( s.test2 > 1 ){ ##ensure that it doesnt go to Wilcox or Kruskal-Wallis
              
              if(length(ggroups) == 1){
                p.val <- NA
                
              } else if(length(ggroups) == 2){
                n=1
                em.btwn.p <- NA
                
                for( m in visitnumbers ){
                  result <- wilcox.test( OUTCOME ~ GROUPING, data = w[w$VISITNUMBER == m, ] )
                  em.btwn.p[n] <- paste0(sprintf("%.5f", result$p.value), " (w)")
                  
                  
                  n=n+1
                }
                cat("#-- Between p-values from Wilcox test", nl)
                print(em.btwn.p) 
                em.btwn <- data.frame( em.btwn.p )
                names( em.btwn ) <- "p.value"
                
                
              } else if(length(ggroups) > 2){
                n=1
                em.btwn.p <- NA
                em.pairwise <- data.frame(AB=NA, AC=NA, BC=NA, AD=NA, BD=NA, CD=NA, AE=NA, BE=NA, CE=NA, DE=NA, AF=NA, BF=NA, CF=NA, DF=NA, EF=NA)
                for( m in visitnumbers ){
                  result <- kruskal.test( OUTCOME ~ GROUPING, data = w[w$VISITNUMBER == m, ] )
                  em.btwn.p[n] <- paste0(sprintf("%.5f", result$p.value), " (k)")
                  
                  if(isTRUE(runpairwise)){
                    #pairwise comparisons
                    #ww <- rank(w[w$VISITNUMBER == m, "CHANGE" ], ties.method="max")
                    posthoc <- PMCMRplus::kwAllPairsNemenyiTest(OUTCOME ~ GROUPING, data = w[w$VISITNUMBER == m, ], p.adjust.method=NULL)
                    print(posthoc)
                    
                    posthoc1 <- as.matrix(posthoc$p.value)
                    posthoc2 <- as.data.frame(as.table(posthoc1))
                    posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
                    posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
                    posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
                    em.pairwise <- posthoc3[c("contrast", "p.value")]
                    print(em.pairwise)
                    
                    if(isTRUE(adj)){
                      em.pairwise.adj <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("contrast", "p.value") )
                      posthoc <- PMCMRplus::kwAllPairsNemenyiTest(OUTCOME ~ GROUPING, data = w[w$VISITNUMBER == m, ], p.adjust.method="single-step")
                      print(posthoc)
                      posthoc1 <- as.matrix(posthoc$p.value)
                      posthoc2 <- as.data.frame(as.table(posthoc1))
                      posthoc3 <- posthoc2[!is.na(posthoc2$Freq), ]
                      posthoc3$contrast <- paste0(posthoc3$Var2, " - ", posthoc3$Var1)
                      posthoc3$p.value <- paste0(n_decimals(posthoc3$Freq, 3), " (k)")
                      em.pairwise.adj <- posthoc3[c("contrast", "p.value")]
                      print(em.pairwise.adj)
                    }
                  }
                  
                  #n=n+1
                }
                
                cat("#-- Between p-values from Kruskal-Wallis test", nl)
                print(em.btwn.p) 
                em.btwn <- data.frame( em.btwn.p )
                names( em.btwn ) <- "p.value"
                
                #EM <- emmeans( final.model, ~GROUPING*VISITNUMBER )
                #final.model2 <- update( EM, infer = c( TRUE, TRUE ))
                #em.within <- data.frame( final.model2 )
                
                #for(q in visitnumbers){
                # within.output <- WithinPaired( w.imp, baselinevisit=baselinevisit, speccomp1=speccomp1, speccomp2=speccomp2, i=q, big.p.val, output )
                # print(data.frame(within.output))
                #for(r in ggroups){
                #w.p.val <- as.numeric(within.output[1, r])
                #  w.p.val <- within.output[1, r]
                #em.within[nrow(em.within) + 1,] <- list(r, q, NA, NA, NA, NA, NA, NA, w.p.val)
                
                #   em.within[em.within$GROUPING == r & em.within$VISITNUMBER == q, "p.value" ] <- w.p.val
                # }
                #cat("#-- Paired t-test/Wilcoxon test p-values inserted --#", nl)
                # print(em.within)
                #}
                
              }
              
            } 
            else {
              
              if(design == "parallel"){
                EM <- emmeans( log.final.model, ~GROUPING*VISITNUMBER )
              } else if(design == "crossover"){
                EM <- emmeans( log.final.model, ~GROUPING*VISITNUMBER + SEQ + PERIOD)
              }
              final.model2 <- update( EM, infer = c( TRUE, TRUE ))
              sum.mod <- summary(log.final.model)
              # print(sum.mod)
              if(log.f.model == "log.fit.pla2"){
                #em.within <- data.frame( final.model2 )
                #return(em.within)
                #print(em.within)
                #print(summary(final.model2))
                EM4 <- joint_tests( final.model2, by = "VISITNUMBER" )
                em.btwn <- data.frame( EM4 )
                cat("#-- Between p-values from Log transform", nl)
                
                rownames(em.btwn) <- paste0("VISITNUMBER", em.btwn$VISITNUMBER)
                colnames(em.btwn)[colnames(em.btwn) == "p.value"] <- "p-value"
                em.btwn$`p-value` <- sprintf("%.5f", em.btwn$`p-value`)
                cat("#-- Between p-values from log transformed test", nl)
                print(em.btwn)
                em.btwn$p.value <- em.btwn$`p-value`
                em.btwn$`p-value` <- ifelse(is.na(em.btwn$`p-value`), NA, paste0(em.btwn$`p-value`, " (l)"))
              } else {
                em.btwn <- as.data.frame( sum.mod$tTable )
                em.btwn$`p-value` <- sprintf("%.5f", em.btwn$`p-value`)
                cat("#-- Between p-values from log transformed test", nl)
                print(em.btwn)
                em.btwn$p.value <- em.btwn$`p-value`
                em.btwn$`p-value` <- ifelse(is.na(em.btwn$`p-value`), NA, paste0(em.btwn$`p-value`, " (l)"))
                
              }
              
              ##pairwise comparisons 
              EM5 <- contrast( final.model2, "pairwise", adjust = NULL )
              em.pairwise <- data.frame( EM5 )
              em.pairwise$p.value <- paste0(n_decimals(em.pairwise$p.value, 3), " (l)")
              EM6 <- contrast( final.model2, "pairwise", adjust = "tukey" )
              em.pairwise.adj <- data.frame( EM6 )
              em.pairwise.adj$p.value <- paste0(n_decimals(em.pairwise.adj$p.value, 3), " (l)")
              
              #within group p-values from the model
              EMW <- emmeans( log.final.model, ~GROUPING*VISITNUMBER)
              final.model2W <- update( EMW, infer = c( TRUE, TRUE ))
              ##pairwise comparisons 
              EMW5 <- contrast( final.model2W, "pairwise", adjust = NULL )
              em.within <- as.data.frame( EMW5 )
              cat("#-- Within-group (between visit) from log transform", nl)
              # print(em.within)
              visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
              visit_pairs1 <- as.data.frame(visit_pairs)
              em.within <- dplyr::bind_cols(em.within, visit_pairs1)
              print(em.within)
              em.within$p.value <- paste0(n_decimals(em.within$p.value, 3), " (l)")
              #adj
              EMW6 <- contrast( final.model2W, "pairwise", adjust = "tukey" )
              em.within.adj <- data.frame( EMW6 )
              visit_pairs <- NULL
              visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
              visit_pairs1 <- as.data.frame(visit_pairs)
              em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
              em.within.adj$p.value <- paste0(n_decimals(em.within.adj$p.value, 3), " (l)")
            }
          }
        } else  {
          
          if(design == "parallel"){
            EM <- emmeans( final.model, ~GROUPING*VISITNUMBER )
          } else if(design == "crossover"){
            EM <- emmeans( final.model, ~GROUPING*VISITNUMBER + SEQ + PERIOD)
          }
          
          final.model2 <- update( EM, infer = c( TRUE, TRUE ))
          
          sum.mod <- summary(final.model)
          if(f.model == "fit.pla2"){
            EM4 <- joint_tests( final.model2, by = "VISITNUMBER" )
            em.btwn <- data.frame( EM4 )
            rownames(em.btwn) <- paste0("VISITNUMBER", em.btwn$VISITNUMBER)
            colnames(em.btwn)[colnames(em.btwn) == "p.value"] <- "p-value"
            em.btwn$`p-value` <- sprintf("%.5f", em.btwn$`p-value`)
            cat("#-- Between p-values from Parametric Test:", nl)
            print(em.btwn)
          } else {
            em.btwn <- as.data.frame( sum.mod$tTable )
            em.btwn$`p-value` <- sprintf("%.5f", em.btwn$`p-value`)
            cat("#-- Between Groups p-values from Parametric Test: ", nl)
            print(em.btwn)
          }
          
          ##pairwise comparisons
          EM5 <- contrast( final.model2, "pairwise", by = "VISITNUMBER", adjust = NULL )
          em.pairwise <- data.frame( EM5 )
          em.pairwise$p.value <- n_decimals(em.pairwise$p.value, 3)
          EM6 <- contrast( final.model2, "pairwise", by="VISITNUMBER", adjust = "tukey" )
          em.pairwise.adj <- data.frame( EM6 )
          em.pairwise.adj$p.value <- n_decimals(em.pairwise.adj$p.value, 3)
          
          #within group p-values from the model
          EMW <- emmeans( final.model, ~GROUPING*VISITNUMBER)
          final.model2W <- update( EMW, infer = c( TRUE, TRUE ))
          ##pairwise comparisons 
          EMW5 <- contrast( final.model2W, "pairwise", adjust = NULL )
          em.within <- as.data.frame( EMW5 )
          cat("#-- Within-group (between visit) p-values from parametric test", nl)
          # print(em.within)
          visit_pairs <- str_split_fixed(em.within$contrast, " - ", 2)
          visit_pairs1 <- as.data.frame(visit_pairs)
          em.within <- dplyr::bind_cols(em.within, visit_pairs1)
          print(em.within)
          em.within$p.value <- n_decimals(em.within$p.value, 3)
          #adj
          EMW6 <- contrast( final.model2W, "pairwise", adjust = "tukey" )
          em.within.adj <- data.frame( EMW6 )
          visit_pairs <- NULL
          visit_pairs <- str_split_fixed(em.within.adj$contrast, " - ", 2)
          visit_pairs1 <- as.data.frame(visit_pairs)
          em.within.adj <- dplyr::bind_cols(em.within.adj, visit_pairs1)
          em.within.adj$p.value <- n_decimals(em.within.adj$p.value, 3)
        }
        
      }
      
    }
    
    
    
    
    
    ## Descriptives ####
    k = 1 #for lsmeans cells
    l = 1 #for between group p-values
    
    for(i in visitnumbers){
      #u <- w.noimp[which( w.noimp$VISITNUMBER == i & !is.na(w.imp$CHANGE) ),]
      u <- w.noimp[which( w.noimp$VISITNUMBER == i ),]
      #return(u)
      
      #u.imp <- w.imp[which( w.imp$VISITNUMBER == i & !is.na(w.imp$CHANGE) ),]
      w <- u.imp <- w.imp[which( w.imp$VISITNUMBER == i ),]
      #return(u.imp)
      chgsumm <- with( u, aggregate(CHANGE, by=list(GROUP = GROUPING), FUN = f) )
      #percent.change <- with( u, aggregate(PERCENT, by=list(GROUP = GROUPING), FUN = f) )
      #percent.change <- q.desc_stats(u, groupvar="GROUPING", outcomevar="PERCENT")
      
      #cat("# ------- ------- CHANGE FROM VISIT ",i," ------- -------- #", nl)
      #print(chgsumm)
      #cat("Percent Change:", nl)
      #print(percent.change)
      #stat.desc(u)
      
      
      
      ##### Output things #####
      if(grps == 1){
        cat("No between group p-value", nl)
        p.val <- NA
      } else {
        vviz <- paste0("VISITNUMBER", i)
        print(vviz)
        # print(em.btwn$`p-value`[row.names(em.btwn) == vviz])
        if("p-value" %in% colnames(em.btwn)){ the.p.val <- em.btwn$`p-value`[row.names(em.btwn) == vviz] } 
        else if("p.value" %in% colnames(em.btwn)) { the.p.val <- em.btwn$p.value[row.names(em.btwn) == vviz] }
        else{the.p.val <- NA}
        
        
        cat("the.p.val", the.p.val, nl)
        if(!is.na(the.p.val)){
          if(substring_right(the.p.val, 3) %in% c("(l)", "(r)", "(w)", "(k)")){
            filesuffix <- substring_right(the.p.val, 3)
            a <- substr(the.p.val, 1, 7)
            a <- as.numeric(a)
            p.val <- n_decimals(a, 3)
            if(p.val == "0.000"){ 
              p.val <- "<0.001"
            }
            
            p.val <- paste0(p.val, " ", filesuffix)
          } else {
            a <- as.numeric(the.p.val)
            p.val <- n_decimals(a, 3)
            p.val <- as.character(p.val)
          }
        } else {
          p.val <- the.p.val
        }
        
      }
      
      
      #p.val <- n_decimals(em.btwn$p.value[l], 3)
      
      #means & SDs
      ms.output.list <- list()
      ms.output.list[["#"]] <- paste0("Change from ", baselinevisit, " to ", i)
      for(j in 1:grps) {
        ms.output.list[[group.names[j]]] <- paste0( n_decimals(chgsumm$x[j,5], 2), " \u00B1 ", n_decimals(chgsumm$x[j,6], 2), " (",chgsumm$x[j,1],")" )
      }
      ms.output.list[["p.val"]] <- p.val
      output[nrow(output) + 1,] = ms.output.list
      
      #medians & ranges
      mr.output.list <- list()
      mr.output.list[["#"]] <- " "
      for(m in 1:grps) {
        mr.output.list[[group.names[m]]] <- paste0( n_decimals(chgsumm$x[m,2], 2), " (", n_decimals(chgsumm$x[m,3], 2), " to ", n_decimals(chgsumm$x[m,4], 2), ")" )
      }
      mr.output.list[["p.val"]] <- " "
      output[nrow(output) + 1,] = mr.output.list
      
      #means, SD & n
      #Group.A <- paste(n_decimals(chgsumm$x[1,5], 2), " \u00B1 ", n_decimals(chgsumm$x[1,6], 2), " (",chgsumm$x[1,1],")", sep="")
      #Group.B <- paste(n_decimals(chgsumm$x[2,5], 2), " \u00B1 ", n_decimals(chgsumm$x[2,6], 2), " (",chgsumm$x[2,1],")", sep="")
      #output[nrow(output) + 1,] = list(Group.A, Group.B, p.val)
      
      #median & range
      #Group.A <- paste(n_decimals(chgsumm$x[1,2], 2), " (", n_decimals(chgsumm$x[1,3], 2), " to ", n_decimals(chgsumm$x[1,4], 2), ")", sep="")
      #Group.B <- paste(n_decimals(chgsumm$x[2,2], 2), " (", n_decimals(chgsumm$x[2,3], 2), " to ", n_decimals(chgsumm$x[2,4], 2), ")", sep="")
      #output[nrow(output) + 1,] = list(Group.A, Group.B, "")
      
      #LSMEANS & SEM
      #return(em.within)
      #Group.A <- paste(n_decimals(em.within$emmean[k], 2), " (",n_decimals(em.within$SE[k], 2),")", sep="")
      #Group.B <- paste(n_decimals(em.within$emmean[k+1], 2), " (",n_decimals(em.within$SE[k+1], 2),")", sep="")
      #output[nrow(output) + 1,] = list(Group.A, Group.B, "")
      
      #within group p-♥alue from model
      if(within.group %in% c("model", "both")){
        mw.output.list <- list()
        mw.output.list[["#"]] <- " "
        
        if(grps == 1){
          within.p <- em.within[em.within$V1 == baselinevisit & em.within$V2 == i, "p.value" ]
          within.p2 <- as.character(within.p)
          
          within.p3 <- NA
          if(substring_right(within.p2, 3) == "(r)"){
            within.p3 <- within.p2
          } else {
            a <- as.numeric(within.p2)
            within.p3 <- n_decimals(a, 3)
            within.p3 <- as.character(within.p3)
          }
          
          mw.output.list[[group.names[group.names[1]]]] <- within.p3
          
        }
        else {
          
          for(o in 1:grps) {
            
            oo <- group.names[o]
            cat("Groups: ", group.names, nl, "GROUP: ", oo, " Baselinevisit: ", baselinevisit, " Visitnumber: ", i)
            within.p <- em.within[em.within$V1 == paste0(oo, " ", baselinevisit) & em.within$V2 == paste0(oo, " ", i), "p.value" ]
            within.p2 <- as.character(within.p)
            cat(nl, "within.p2 Group ", oo, " is ", within.p2, nl, nl)
            
            within.p3 <- NA
            if(substring_right(within.p2, 3) == "(r)"){
              within.p3 <- within.p2
            } else {
              a <- as.numeric(within.p2)
              within.p3 <- n_decimals(a, 3)
              within.p3 <- as.character(within.p3)
            }
            
            mw.output.list[[oo]] <- within.p3
          }
        }
        
        mw.output.list[["p.val"]] <- " "
        output[nrow(output) + 1,] = mw.output.list
        
      }
      
      
      
      
      ## within groups paired t-test ##
      if(within.group %in% c("t-test", "both")){
        # output <- WithinPaired(w.imp, baselinevisit, speccomp1, speccomp2, i, big.p.val, assume.normal.dist, ggroups, grps, output)
        output <- WithinPaired(w.imp, baselinevisit, speccomp1, speccomp2, i, assume.normal.dist, ggroups, grps, output)
        
      }
      
      
      
      
      
      ##percent changes commented out on Oct 16, 2019. study doesn't require percent changes in stats report
      mysumm <- with( w.noimp[which( w.noimp$VISITNUMBER == baselinevisit ),], aggregate(OUTCOME, by=list(GROUP = GROUPING), FUN = f) )
      percent.output.list <- list()
      for(q in 1:grps) {
        percent.raw <- (chgsumm$x[q,5] / mysumm$x[q,5]) * 100
        percent.output.list[[group.names[q]]] <- paste0(n_decimals(percent.raw, 1), "%")
      }
      output.pchange[nrow(output.pchange) + 1,] = percent.output.list
      #percent.output.list[["p.val"]] <- " "
      #output[nrow(output) + 1,] = percent.output.list
      
      
      k=k+6
      l=l+1
      
      # if(isTRUE(runpairwise)){
      #   
      #   output.pairwise[nrow(output.pairwise) + 1,] = list(paste0("Change from ", baselinevisit, " to ", i))
      #   
      #   #unadjusted
      #   unadj.output.list <- list()
      #   unadj.output.list[["#"]] <- "Pairwise unadj"
      #   for(q in 1:grps) {
      #     unadj.output.list[[group.names[q]]] <- paste0(em.pairwise$p.value[q], " (", em.pairwise$contrast[q], ")")
      #   }
      #   unadj.output.list[["p.val"]] <- ""
      #   output[nrow(output) + 1,] = unadj.output.list
      #   output.pairwise[nrow(output.pairwise) + 1,] = unadj.output.list
      #   
      #   
      #   if(isTRUE(adj)){
      #     adj.output.list <- list()
      #     adj.output.list[["#"]] <- "Pairwise adj"
      #     for(r in 1:grps) {
      #       adj.output.list[[group.names[r]]] <- paste0(em.pairwise.adj$p.value[r], " (", em.pairwise.adj$contrast[r], ")")
      #     }
      #     adj.output.list[["p.val"]] <- ""
      #     output[nrow(output) + 1,] = adj.output.list
      #     output.pairwise[nrow(output.pairwise) + 1,] = adj.output.list
      #   }
      #   
      #   
      # }
      
      
      # if(isTRUE(runpairwise)){
      #   sub.empairwise <- em.pairwise[em.pairwise$VISITNUMBER == i,]
      #   sub.empairwise.adj <- em.pairwise.adj[em.pairwise.adj$VISITNUMBER == i,]
      #   
      #   output.pairwise[nrow(output.pairwise) + 1,] = list(paste0("Change from ", baselinevisit, " to ", i))
      #   unadj.output.list <- list()
      #   adj.output.list <- list()
      #   
      #   unadj.output.list[["#"]] <- "Pairwise unadj"
      #   for(q in 1:grps) {
      #     unadj.output.list[[group.names[q]]] <- paste0(sub.empairwise$p.value[q], " (", sub.empairwise$contrast[q], ")")
      #   }
      #   unadj.output.list[["p.val"]] <- " "
      #   output[nrow(output) + 1,] = unadj.output.list
      #   output.pairwise[nrow(output.pairwise) + 1,] = unadj.output.list
      #   
      #   if(isTRUE(adj)){
      #     adj.output.list[["#"]] <- "Pairwise adj"
      #     for(r in 1:grps) {
      #       adj.output.list[[group.names[r]]] <- paste0(sub.empairwise.adj$p.value[q], " (", sub.empairwise.adj$contrast[q], ")")
      #     }
      #     adj.output.list[["p.val"]] <- " "
      #     output[nrow(output) + 1,] = adj.output.list
      #     output.pairwise[nrow(output.pairwise) + 1,] = adj.output.list
      #   }
      # }
      
      
      # 
      # if(isTRUE(runpairwise)){
      #   em.pairwise$timepoint <- i
      #   
      #   if(isTRUE(adj) & is.null(output.pairwise.adj)){
      #     em.pairwise.adj$timepoint <- i
      #   }
      #   
      #   
      #   if(is.null(output.pairwise)){
      #     output.pairwise <- em.pairwise
      #     
      #     if(isTRUE(adj)){
      #       output.pairwise.adj <- em.pairwise.adj
      #     }
      #     
      #     
      #   } else {
      #     output.pairwise <- merge(output.pairwise, em.pairwise, all = T)
      #     
      #     if(isTRUE(adj)){
      #       output.pairwise.adj <- merge(output.pairwise.adj, em.pairwise.adj, all = T)
      #     }
      #     
      #     
      #   }
      #   
      # }
      
      
    }
    
    
    
    
    #Run pairwise comparisons?
    #if(isTRUE(runpairwise)){
    #  write.csv(output.pairwise, file = paste(output_path, "/", outcomevar, filesuffix, "_Chg_pairwise.csv", sep=""))
    #  #print(em.pairwise)
    
    #  payload.pairwise[[paste0(outcomevar, filesuffix)]] <- output.pairwise
    #}
    
    
    # if(isTRUE(runpairwise)){
    #   
    #   if(is.null(output.pairwise)){
    #     output.pairwise <- em.pairwise
    #     
    #     if(isTRUE(adj)){
    #       output.pairwise.adj <- em.pairwise.adj
    #     }
    #     
    #     
    #   } else {
    #     output.pairwise <- merge(output.pairwise, em.pairwise, all = T)
    #     
    #     if(isTRUE(adj)){
    #       output.pairwise.adj <- merge(output.pairwise.adj, em.pairwise.adj, all = T)
    #     }
    #     
    #     
    #   }
    #   
    # }
    
    
    
    #Export main output
    #write.csv(output, file = paste0(output_path, "/", outcomevar, filesuffix,".csv"))
    #payload[[paste0(outcomevar, filesuffix)]] <- output
    
    
    
    #Export percent change output
    #write.csv(output.pchange, file = paste0(output_path, "/", outcomevar, "_PcentChg.csv"))
    #payload.pchange[[paste0(outcomevar, filesuffix)]] <- output.pchange
    
    if(isTRUE(runpairwise)){
      # print(em.pairwise)
      # print(em.pairwise.adj)
      
      
      output.pairwise <- em.pairwise
      
      
      
      if(isTRUE(adj)){
        output.pairwise.adj <- em.pairwise.adj
        
        
      } 
      
    }
    #Export pairwis♥e comparisons
    if(isTRUE(runpairwise)){
      # output.pairwise <- em.pairwise[c("contrast", "VISITNUMBER", "p.value")]
      # if(isTRUE(adj)){
      #   output.pairwise <- em.pairwise.adj[c("contrast", "VISITNUMBER", "p.value")]
      # }
      
      output.pairwise.title <- paste0("Pairwise comparisons for ", outcomevar, " on ", baselinevisit ," and ", toString.default(visitnumbers), "; change in ", outcomevar, " from ", baselinevisit ," to ", toString.default(visitnumbers), " in the ",popn ," population")
      payload[[paste0("CHG TITLE: ", outcomevar, filesuffix, "pairwise_unadj")]] <- output.pairwise.title
      payload[[paste0("CHG ", outcomevar, filesuffix, "pairwise_unadj")]] <- output.pairwise
      
      if(isTRUE(adj)){
        # output.pairwise.adj <- em.pairwise.adj
        payload[[paste0("CHG TITLE (ADJ): ", outcomevar, filesuffix, "pairwise_adj")]] <- paste0("Adjusted,", output.pairwise.title)
        payload[[paste0("CHG (ADJ) ", outcomevar, filesuffix, "pairwise_adj")]] <- output.pairwise.adj
      }
    }
    
    
    #E♣port percent change output
    output.pchange <- q.desc_stats(w.noimp[w.noimp$VISITNUMBER != baselinevisit,], outcomevar="PERCENT", groupvar="GROUPING", timevar="VISITNUMBER")

    output.pchange.title <- paste0("Percent changes for ", outcomevar, " from ", baselinevisit ," to ", toString.default(visitnumbers), " in the ",popn ," population")
    payload[[paste0("TITLE: ", outcomevar, filesuffix, "pchange")]] <- output.pchange.title
    payload[[paste0(outcomevar, filesuffix, "pchange")]] <- output.pchange
    
    
    #Export main output
    output <- RenameHeaders(output)
    output.title <- paste0(outcomevar, " on$$at ", baselinevisit ," and ", toString.default(visitnumbers), "; change in ", outcomevar, " from ", baselinevisit ," to ", toString.default(visitnumbers), " in the ",popn ," population")
    payload[[paste0("TITLE: ", outcomevar, filesuffix)]] <- output.title
    payload[[paste0(outcomevar, filesuffix)]] <- output
    cat(green("v ") %+% blue("SUMMARY"), nl)
    # print(payload)
    
    
    
    # result <- list(payload, payload.pairwise, payload.pchange)
    
    #Export to file 
    if(".csv" %in% exportfile){
      # utils::write.csv(output, 
      #                  file = paste(exppath, outcomevar, "_", filesuffix, ".csv", sep=""))
      # if(isTRUE(runpairwise)){
      #   utils::write.csv(output.pairwise, 
      #                    file = paste(exppath, outcomevar, "_", filesuffix, 
      #                                 "_Pairwise.csv", sep=""))
      # }
      
      utils::write.csv(output, 
                       file = paste(exppath, outcomevar, filesuffix, ".csv", sep=""))
      if(isTRUE(runpairwise)){
        utils::write.csv(output.pairwise, 
                         file = paste(exppath, outcomevar, filesuffix, 
                                      "_Pairwise.csv", sep=""))
      }
      
      utils::write.csv(output.pchange, 
                       file = paste(exppath, outcomevar, filesuffix, "_PercentChange_", ".csv", sep=""))
    }
    
    
    if(".doc" %in% exportfile){
      q.write.to.word(payload, exportpath=exportpath, 
                      docname=paste0(outcomevar, filesuffix))
      
      # if(isTRUE(runpairwise)){
      #   q.write.to.word(payload.pairwise, exportpath=exportpath, 
      #                 docname=paste0(outcomevar, filesuffix, "_Pairwise_"))
      # }
      # 
      # q.write.to.word(payload.pchange, exportpath=exportpath, 
      #                 docname=paste0(outcomevar, filesuffix, "_PercentChange_"))
    }
    
    toc(); cat(nl);
    return(payload)
    # return(result)
  }
  #'
  #'
  #'
  #'
  #####6. Within group analysis for sensitivity testing #####
  # WithinPaired <- function( w.imp, baselinevisit, speccomp1, speccomp2, i, big.p.val, assume.normal.dist, ggroups, grps, output ){
  WithinPaired <- function( w.imp, baselinevisit, speccomp1, speccomp2, i, assume.normal.dist, ggroups, grps, output ){ #removed big.p.val
    #ggroups <- w.imp$GROUPING
    #ggroups <- unique(ggroups)
    #cat("GGROUPS: ", ggroups, nl)
    #cat("GRPS: ", grps, nl)
    ggroups <- as.vector(ggroups)
    #cat("GGROUPS: ", ggroups, nl)
    #cat("GRPS: ", grps, nl)
    
    within.p.vals <- list()
    within.p.vals[["#"]] <- ""
    
    
    #print("DB sent to WithinPaired function: ")
    #print(summary(w.imp))
    
    for(j in c(1:grps)){
      v <- w.imp[ which( w.imp$GROUPING == ggroups[j] ),] #use imputed data for p-values
      #print(v)
      
      if(!is.na(speccomp1) & !is.na(speccomp2)){
        special <- paste0(speccomp1, speccomp2)
        #print("Special Visit:")
        # print(special)
        #print(i)
        if(i == special){
          u3 <- v[v$VISITNUMBER == speccomp1 | v$VISITNUMBER == speccomp2,]
          #print(head(u3))
          #make sure data is paired
          first_set <- u3[u3$VISITNUMBER == speccomp1 & !is.na(u3$OUTCOME), c("SUBJECTNUM") ]
          second_set <- u3[u3$VISITNUMBER == speccomp2 & !is.na(u3$OUTCOME), c("SUBJECTNUM") ]
          u4 <- u3[u3$SUBJECTNUM %in% first_set,]
          u5 <- u4[u4$SUBJECTNUM %in% second_set,]
          #return(u5)
          #print("Special Set:")
          #print(head(u5))
          
        } else {
          u3 <- v[which(v$VISITNUMBER == baselinevisit | v$VISITNUMBER == i),]
          #print(summary(u3))
          #make sure data is paired
          first_set <- u3$SUBJECTNUM[u3$VISITNUMBER == baselinevisit & !is.na(u3$OUTCOME)]
          second_set <- u3$SUBJECTNUM[u3$VISITNUMBER == i & !is.na(u3$OUTCOME)]
          u4 <- u3[u3$SUBJECTNUM %in% first_set,]
          u5 <- u4[u4$SUBJECTNUM %in% second_set,]
          #return(u5)
          #print("Regular Set1 (U5):")
          #print(head(u5))
          
        }
      } 
      else {
        u3 <- v[(v$VISITNUMBER == baselinevisit | v$VISITNUMBER == i),]
        #(summary(u3))
        #make sure data is paired
        first_set <- u3[u3$VISITNUMBER == baselinevisit & !is.na(u3$OUTCOME), c("SUBJECTNUM") ]
        second_set <- u3[u3$VISITNUMBER == i & !is.na(u3$OUTCOME), c("SUBJECTNUM") ]
        u4 <- u3[u3$SUBJECTNUM %in% first_set,]
        u5 <- u4[u4$SUBJECTNUM %in% second_set,]
        # print("Regular Set2 (U5):")
        # print("Baseline visit")
        # print(length(first_set))
        # print(length(u5$SUBJECTNUM[u5$VISITNUMBER == baselinevisit]))
        # print("Other visit")
        # print(length(second_set))
        # print(length(u5$SUBJECTNUM[u5$VISITNUMBER == i]))
        # #print(head(u5))
        #print(table(u5$SUBJECTNUM, u5$VISITNUMBER))
        #return(u5)
      }
      
      
      #summary(u5)
      n.outcomes <- unique(u5$OUTCOME)
      n.outcomes <- n.outcomes[!is.nan(n.outcomes) & !is.na(n.outcomes) & !is.infinite(n.outcomes)]
      
      
      if(isTRUE(assume.normal.dist)){
        if(length(n.outcomes) > 2){
          result <- t.test(OUTCOME ~ VISITNUMBER, data = u5, paired = TRUE, conf.level = 0.95, na.rm = TRUE)
          p.val <- n_decimals(result$p.value, 3)
          if(p.val == "0.000"){ p.val = "<0.001" }
        } else {
          p.val <- n_decimals(NA, 3)
        }
        
        
      }
      else{
        if(length(n.outcomes) > 2){
          s.test3 <- shapiro.test(u5$OUTCOME)$p.value
        } else {
          s.test3 <- 0.0001
        }
        
        #s.test3 <- shapiro.test(u5$OUTCOME)$p.value
        #cat("Within Shapiro Test Group ", ggroups[j], ": ", s.test3, nl)
        if(exists("s.test3")) cat(silver("  Within Group Shapiro Test for raw data (" %+% as.character(ggroups[j]) %+% "): " %+% as.character(round(s.test3, 3))), nl)
        
        if( s.test3 < 0.01){
          u5$OUTCOME1 <- log10(u5$OUTCOME + 1 - min(u5$OUTCOME))
          if(length(n.outcomes) > 2){
            s.test4 <- try(shapiro.test(u5$OUTCOME1)$p.value)
          } else {
            s.test4 <- 0.0001
          }
          #s.test4 <- try(shapiro.test(u5$OUTCOME1)$p.value)
          
          if( s.test4 < 0.01 ){
            result <- wilcox.test(OUTCOME ~ VISITNUMBER, data = u5, paired = TRUE, conf.level = 0.95, na.rm = TRUE)
            p.val <- n_decimals(result$p.value, 3)
            if(p.val == "0.000"){ p.val = "<0.001" }
            p.val <- paste0(p.val," (w)")
          } else {
            result <- t.test(OUTCOME1 ~ VISITNUMBER, data = u5, paired = TRUE, conf.level = 0.95, na.rm = TRUE)
            p.val <- n_decimals(result$p.value, 3)
            
          }
          
        }
        else  {
          result <- t.test(OUTCOME ~ VISITNUMBER, data = u5, paired = TRUE, conf.level = 0.95, na.rm = TRUE)
          p.val <- n_decimals(result$p.value, 3)
          if(p.val == "0.000"){ p.val = "<0.001" }
        }
      }
      
      #big.p.val[ggroups[j]] <- p.val
      within.p.vals[[ggroups[j]]] <- p.val
    }
    
    
    
    #big.p.val[grps+1] <- " "
    within.p.vals[["p.val"]] <- " "
    #output[nrow(output) + 1,] = big.p.val
    output[nrow(output) + 1,] = within.p.vals
    
    return(output)
  }
  #
  #
  #####7. Categorical variables for GEE analysis ####
  SumCatGEE <- function(db, outcomevar, ggroups=NULL, filesuffix){
    db["OUTCOME"] <- db[outcomevar]
    if(is.null(ggroups)){
      ggroups <- db$GROUPING
      ggroups <- unique(ggroups)
      grps <- length(ggroups)
    } else {
      grps <- length(ggroups)
    }
    grps
    ggroups <- sort(ggroups)
    #grp.names <- c("ABC", "BCA", "CAB", "D", "E", "F", "G", "H", "I", "J")
    grp.names <- ggroups 
    group.names <- as.vector(grp.names[1:grps])
    #print(group.names)
    output <- setNames(data.frame(matrix(ncol = grps+2, nrow = 0)), c("Variable", group.names, "p.val") )
    
    p.val <- NULL
    vvists <- db$VISITNUMBER
    vvists <- unique(na.omit(vvists))
    cat("VISITS: ", vvists, nl)
    num.outcomes <- length(unique(db$OUTCOME))
    
    for(vvist in vvists){
      db2 <- db[db$VISITNUMBER == vvist,]
      result <- with(db2, table(OUTCOME, GROUPING))
      result.freq <- data.frame(result)
      
      if(num.outcomes > 1){
        result.summ <- summary(result)
        result.summ.p <- result.summ$p.value
        
        result.fisher <- fisher.test(result, simulate.p.value=FALSE)
        result.fisher.p <- result.fisher$p.value
      } else {
        result.fisher.p <- 1
        result.summ.p <- 1
      }
      result.perc <- round(100* prop.table(result, 2), 1)
      result.percent <- data.frame(result.perc)
      result.percent$Freq <- ifelse(is.nan(result.percent$Freq), 0.00, result.percent$Freq)
      
      print(result.freq)
      print(result.percent)
      outcome1 <- unique(db$OUTCOME)
      outcome1 <-  sort(outcome1)
      #cat("Outcomes1: ", outcome1, nl)
      
      outcome <- outcome1
      min.freq <- min(result.freq$Freq)
      
      cat("Outcomes: ", outcome, nl)
      cat("Min cell: ", min.freq, nl)
      cat("Fisher p-value: ", result.fisher.p, nl)
      cat("Chi p-value: ", result.summ.p, nl)
      
      #output the p-value
      p.val <- ifelse(min.freq <= 5, result.fisher.p, result.summ.p )
      p.val <- n_decimals(p.val, 3)
      p.val <- ifelse(p.val < 0.001, "<0.001", p.val)
      cat("Final p-value: ", p.val, nl)
      p.output.list <- list()
      #p.output.list[["Variable"]] <- paste0("Visit ", vvist)
      p.output.list[["Variable"]] <- paste0(vvist)
      for(k in 1:grps) {
        p.output.list[[group.names[k]]] <- " "
      }
      p.output.list[["p.val"]] <- p.val
      output[nrow(output) + 1, ] = p.output.list
      
      for(i in outcome){
        output.list <- list()
        output.list[["Variable"]] <- i
        for(j in 1:grps) {
          output.list[[group.names[j]]] <- paste0(result.freq$Freq[result.freq$OUTCOME == i & result.freq$GROUPING == grp.names[j]],
                                                  " (", n_decimals(result.percent$Freq[result.percent$OUTCOME == i & result.percent$GROUPING == grp.names[j]], 2), "%)")
        }
        output.list[["p.val"]] <- " "
        
        output[nrow(output) + 1,] = output.list
      }
    }
    
    #payload[[paste0(outcomevar, filesuffix)]] <- output
    
    return(output)
    #write.csv(output, file = paste(output_path, "/", outcomevar,"_", grps, "_Cat.csv", sep=""))
  }
  #
  #
  # Helper functions end here #
  
  
  
  
  
  
  
  #♣ CALL MAIN ANALYSIS FUNCTION HERE ♣####
  cat( "+-----------------------------+", nl)
  cat(green("ANALYZE " %+% as.character(outcomevar)) %+% nl)
  cat( "+-----------------------------+", nl)
  cat(blue("Design: ") %+% silver(design), nl)
  tic()
  
  
  
  if(within.group == "none" | within.group == ""){
    demographics = T
  } else {
    demographics = F
  }
  
  # payload <<- payload.pairwise <<- payload.pchange <<-  q.newPayload()
  
  # payload <-  q.newPayload()
  
  if(design %in% c("parallel", "crossover")){
    result <- AnalyzeCombined( db=db, design=design, outcomevar=outcomevar, idvar=idvar, groupvar=groupvar, 
                               timevar=timevar, visitnumbers=visitnumbers, baselinevisit=baselinevisit, 
                               speccomp1=speccomp1, speccomp2=speccomp2, lloq=lloq, mimp=mimp, locf=locf,  noimp=noimp, 
                               demographics=demographics, runpairwise=runpairwise, adj=adj, within.group=within.group, covs=covs, 
                               assume.normal.dist=assume.normal.dist, useranks=useranks, useglm=useglm, usegee=usegee, glmgeefamily=glmgeefamily, 
                               dbexport=dbexport, filesuffix=filesuffix) #repeated=repeated,
    
    
    
  } else {
    stop("Please specify study design. Options = c('parallel', 'crossover')")
  }
  
  
  # RETURN ANALYSIS OUTPUT ####
  # print(result)
  return(result)
  
}


