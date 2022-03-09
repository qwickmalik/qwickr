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
#'   \item{\code{SUBJECTNUM}}{integer: Unique subject ID}
#'   \item{\code{GROUPING}}{character: Study group/arm/treatment/IP}
#'   \item{\code{VISITNUMBER}}{integer: Visit number}
#'   \item{\code{BIOMARKER}}{double: Biomarker used to assess study endpoints} 
#'}
"rmdata"

#' @title Dummy Categorical Dataset
#' @description Dummy dataset with categorical outcomes. One outcome is pain 
#' frequency at the end of the study (Visit 2). This is entered as frequency 
#' category ("PAIN_FREQUENCY") and recoded as a numeric score ("PAIN_FREQUENCY_SCORE"). 
#' All participants had pain severity of "Very Often" at baseline or Visit 1 
#' (this data is not included). Based on pain frequency at the end of the study, 
#' participants are classified as having improved (1) or not (0) ("PAIN_IMPROVED")
#' @format A data frame with 40 rows and 8 variables:
#' \describe{
#'   \item{\code{SUBJECTNUM}}{integer: Unique subject ID}
#'   \item{\code{AGE}}{integer: age of participants} 
#'   \item{\code{SEX}}{character:sex of participants} 
#'   \item{\code{GROUPING}}{character: Study group/arm/treatment/IP}
#'   \item{\code{VISITNUMBER}}{integer: Visit number}
#'   \item{\code{PAIN_FREQUENCY}}{character: pain frequency category} 
#'   \item{\code{PAIN_FREQUENCY_SCORE}}{integer: pain frequency score} 
#'   \item{\code{PAIN_IMPROVED}}{integer: pain frequency improvement status} 
#'}
"catdata"