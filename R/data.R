#' @title Dummy PK Dataset
#' @description Dummy PK dataset for a 2x2 crossover study involving THC. Study 
#' subjects were randomized to receive two products, A and B in randomly assigned 
#' sequences AB or BA. THC levels were measured in their blood immediately before 
#' product consumption, and at 10 time points after consumption.
#' @format A data frame with 264 rows and 9 variables:
#' \describe{
#'   \item{\code{SUBJECTNUM}}{integer Unique subject ID}
#'   \item{\code{CONC}}{double Concentration of analyte (ng/mL)}
#'   \item{\code{TIME}}{double Time (h)}
#'   \item{\code{SEQ}}{character Treatment sequence ("AB" or "BA")}
#'   \item{\code{PERIOD}}{integer Study period (1 or 2)}
#'   \item{\code{TRT}}{character Treatment/Investigational Product received ("A" or "B")}
#'   \item{\code{GROUPING}}{character Study group. Usually the same as TRT}
#'   \item{\code{ANALYTE}}{character Analyte being measured}
#'   \item{\code{ANALYTEN}}{integer Numeric coding for analyte} 
#'}
"pkdata"

#' @title Dummy Repeated Measures Dataset
#' @description Dummy repeated measures dataset.
#' @format A data frame with 24 rows and 4 variables:
#' \describe{
#'   \item{\code{SUBJECTNUM}}{integer Unique subject ID}
#'   \item{\code{GROUPING}}{character Study group/arm/treatment/IP}
#'   \item{\code{VISITNUMBER}}{integer COLUMN_DESCRIPTION}
#'   \item{\code{BIOMARKER}}{double Biomarker used to assess study endpoints} 
#'}
"rmdata"