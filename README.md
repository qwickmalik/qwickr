# qwickr
A quick and easy way to summarize clinical trial and other similar data, including hypothesis testing and reporting results in Excel and Word formats
# Installation
qwickr is still in development and therefore is not on CRAN as yet. However, users may install the development version using 
```r
remotes::install_github("rstudio-education/gradethis")
```
# Use
Load the package using 
```r
library(qwickr)
```
The main functions in this package are prefixed with qwickr., while helper functions are prefixed with q.

# Example 
```r
SUBJECTNUM <- rep(1:6, each=4)
OUTCOME <- c(rnorm(24,25,6))
VISITNUMBER <- as.factor(c(rep(1:4,6)))
GROUPING <- rep(c(rep("A",4), rep("B",4)), 3)
q.data <- cbind.data.frame(SUBJECTNUM, OUTCOME, VISITNUMBER, GROUPING)
qwickr.cont(db=q.data, resultfield="OUTCOME", groupvar="GROUPING", 
baselinevisit="1", visitnumbers=c(2:4), 
mimp="", locf=TRUE,  noimp=TRUE,
runpairwise=FALSE, adj=FALSE, within.group="t-test", covs="FFFFTF", 
assume.normal.dist=FALSE, useranks=TRUE, useglm=FALSE, usegee=FALSE, glmgeefamily=NULL, 
dbexport="", suffix="mysuffix", design="parallel")
```
