% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qwickr.cont.R
\name{qwickr.cont}
\alias{qwickr.cont}
\title{Summarize continuous data and test hypotheses}
\usage{
qwickr.cont(db, design="parallel", outcomevar="", idvar="", groupvar="", 
timevar="", visitnumbers=c(), baselinevisit="", speccomp1=NA, speccomp2=NA, 
lloq=NA, mimp="cart", locf=FALSE,  noimp=TRUE, runpairwise=FALSE, adj=FALSE,
within.group="t-test", covs="FFFFTF", assume.normal.dist=FALSE, 
useranks=FALSE, useglm=FALSE, usegee=FALSE, glmgeefamily=NULL,
exportfile=c(".doc"), exportpath="", dbexport="", filesuffix="")
}
\arguments{
\item{db}{Data frame}

\item{design}{specify the study design. Options: c("parallel", "crossover").}

\item{outcomevar}{Name of outcome variable \code{(string)}}

\item{idvar}{Name of the unique subject/particpant ID variable \code{(string)}}

\item{groupvar}{Name of grouping variable \code{(string)}}

\item{timevar}{Name of the time variable \code{(string)}}

\item{visitnumbers}{A vector of visit numbers to be included in analysis, excluding reference visit (baseline/screening)}

\item{baselinevisit}{Reference visit number (baseline/screening)}

\item{speccomp1}{Special visit numbers to compare in addition to comparison to baseline. must be a subset of visitnumbers. speccomp1 is the baseline to which speccomp2 is compared. e.g. to compare visits 3 and 4, speccomp1=3, speccomp2=4}

\item{speccomp2}{Special visit numbers to compare in addition to comparison to baseline. must be a subset of visitnumbers. speccomp1 is the baseline to which speccomp2 is compared. e.g. to compare visits 3 and 4, speccomp1=3, speccomp2=4}

\item{lloq}{value to impute everywhere there's a value less than the lower limit of quantification, denoted by -33}

\item{mimp}{conduct multiple imputation? If yes, specify imputation method for multiple imputations (default: Classification and Regression Trees (cart)). See help docs for MICE package for more options. If multiple imputation not desired, use ""}

\item{locf}{conduct LOCF imputation? (T/F)}

\item{noimp}{run analysis for non-imputed dataset? (default=T)}

\item{runpairwise}{conduct between-group pairwise comparisons? (T/F)}

\item{adj}{adjust alpha for between-group pairwise comparisons? (T/F)}

\item{within.group}{calculate change in outcome from ‘baselinevisit’ to all other visits and conduct within-group paired comparisons using the mixed model (option: "model"), paired t-tests/Wilcoxon Signed Rank Test (option: "t-test"), both (option: "both"). If change in outcome from ‘baseline’ is not desired, no within-group comparisons will be done (option: "none")}

\item{covs}{Covariance structures to test for mixed-models.Options: Compound Symmetry (T/F), Heterogeneous CS (T/F), #Autoregressive (T/F), Heterogeneous Autoregressive (T/F), no covariance structure (T/F), no covariance structure using lme4::lmer() (T/F). Default: \code{"FFFFTF"}}

\item{assume.normal.dist}{assume that the data is normally distributed? (T/F)}

\item{useranks}{in the case of intractable non-normality & \code{assume.normal.dist=F}, use ranked ANOVA instead of Wilcoxon/Kruskal Wallis (T/F)}

\item{useglm}{analyze raw data using Poisson distribution OR perform logistic regression? (T/F). Specify appropriate \code{glmgeefamily}}

\item{usegee}{analyze raw data using General Estimating Equations? (T/F). Specify appropriate \code{glmgeefamily}}

\item{glmgeefamily}{specify which family to use e.g. for logistic regression, use \code{"binomial"} and set \code{useglm=T}; for Poisson, use \code{"poisson"}}

\item{exportfile}{Export the output to file? Options: \code{c(".csv", ".doc")}. See \code{q.write.to.word, stats::write.csv }}

\item{exportpath}{Path relative to the working directory where exported files will be saved e.g. "OUTPUT" Do not begin or end with a backslash. If left empty, file will be exported to the working directory.}

\item{dbexport}{specify which dataset to be exported. Options: c("noimp", "locf", "mimp", "")}

\item{filesuffix}{filesuffix to be included in the file name for the exported output file}
}
\value{
Returns a list containing data frame of means, standard deviations, medians, minimum-maximum ranges for each study arm and an associated p-value for each study time point in a parallel or crossover, repeated measures or non-repeated measures design.
}
\description{
Analyze continuous data and test hypotheses
}
\details{
Analyze continuous data and test hypotheses
}
\examples{
q.data <- rmdata
qwickr.cont(db=q.data, design="parallel", outcomevar="BIOMARKER", idvar="SUBJECTNUM",
groupvar="GROUPING", timevar="VISITNUMBER", baselinevisit="1", visitnumbers=c(2:4), 
mimp="", locf=TRUE,  noimp=TRUE,
runpairwise=FALSE, adj=FALSE, within.group="t-test", covs="FFFFTF", 
assume.normal.dist=FALSE, 
useranks=TRUE, useglm=FALSE, usegee=FALSE, glmgeefamily=NULL, 
exportfile=c(".doc"), exportpath="", dbexport="", filesuffix="mysuffix")
}
\author{
Abdul Malik Sulley
}
