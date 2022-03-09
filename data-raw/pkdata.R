## code to prepare `pkdata` dataset goes here
db1 <- datasets::Theoph[c("Subject", "conc")]
db1$TIME <- rep(c(0, 0.25, 0.5, 1, 2, 4, 5, 7, 9, 12, 24), 12)
db1$SEQ <- rep(sort(rep(c("AB", "BA"), 11)), 6)
db1$PERIOD <- 1
db1$TRT <- substr(db1$SEQ, 1, 1)
db1$GROUPING <- paste("Group", db1$TRT)
db1$Subject <- as.character(db1$Subject)
db1$conc <- db1$conc*(as.numeric(db1$Subject)/10)
colnames(db1)[colnames(db1) == "Subject"] <- "SUBJECTNUM"
colnames(db1)[colnames(db1) == "conc"] <- "CONC"
db2 <- db1
db2$PERIOD <- 2
db2$TRT <- substr(db2$SEQ, 2, 2)
db2$GROUPING <- paste("Group", db2$TRT)
db <- rbind.data.frame(db1, db2)
db$ANALYTE <- "THC"
db$ANALYTEN <- 1
db$CONC[db$PERIOD == 2] <- round((db$CONC[db$PERIOD == 2]+0.05)*0.3, 3)

# write.csv(db, file="data-raw/pkdata.csv")
# pkdata <- read.csv("data-raw/pkdata.csv")
pkdata <- as.data.frame(db)
usethis::use_data(pkdata, overwrite = TRUE)




## code to prepare `rmdata` dataset goes here
SUBJECTNUM <- rep(1:6, each=4)
GROUPING <- rep(c(rep("A",4), rep("B",4)), 3)
VISITNUMBER <- as.factor(c(rep(1:4,6)))
BIOMARKER <- c(rnorm(24,25,6))
BIOMARKER

rmdata <- cbind.data.frame(SUBJECTNUM, GROUPING, VISITNUMBER, BIOMARKER)
usethis::use_data(rmdata, overwrite = TRUE)
# file.create("R/rmdata.R")
sinew::makeOxygen(rmdata, add_fields = "source")



#Dummy heart rate data
rtnorm <- function(n, mean = 0, sd = 1, min = 0, max = 1) {
  bounds <- pnorm(c(min, max), mean, sd)
  u <- runif(n, bounds[1], bounds[2])
  qnorm(u, mean, sd)
}

SUBJECTNUM <- rep(1:30, each=4)
GROUPING <- rep(c(rep("A",4), rep("B",4)), 15)
VISITNUMBER <- as.factor(c(rep(1:4,6)))
rmdata <- cbind.data.frame(SUBJECTNUM, GROUPING, VISITNUMBER)
rmdata$BIOMARKER <- NA
rmdata <- rmdata[order(rmdata$VISITNUMBER, rmdata$GROUPING),]

mysamp <- rtnorm(n=120, mean=80, sd=80/10, min=60, max=100)
hist(mysamp, 30)

rmdata$BIOMARKER <- mysamp
usethis::use_data(rmdata, overwrite = TRUE)
# file.create("R/rmdata.R")


