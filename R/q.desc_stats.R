#' @title Generate descriptive summaries for continuous data
#' @aliases q.desc_stats
#' @description Generate descriptive summaries for continuous data namely mean, standard deviation, number of observations, median, minimum, maximum
#' @usage q.desc_stats(x, groupvar="", outcomevar="", timevar="")
#' @param x data frame
#' @param groupvar Variable by which to group data when generating descriptive summaries
#' @param outcomevar Outcome variable name. Variable itself must be at least numeric
#' @param timevar Time/visit variable name. 
#' @return Returns a tibble of results
#' @author Abdul Malik Sulley
#' @examples 
#' id <- rep(1:5, each=4)
#' baseline_values <- rep(rnorm(5,15,4), each=4)
#' outcome <- c(rnorm(20,25,5))
#' visit <- as.factor(c(rep(1:4,5)))
#' q.data <- cbind.data.frame(id, baseline_values, outcome, visit)
#' q.desc_stats(q.data, groupvar="visit", outcomevar="outcome", timevar="visit")
#' @import stats
#' @importFrom stringr "%>%"
#' @export
#' 
#' 
q.desc_stats <- function(x, groupvar="", outcomevar="", timevar="") {
  TIMEPOINT <- OUTCOMEE <- GROUPINGG <- NULL
  if(timevar != ""){x[, "OUTCOMEE"] <- x[, outcomevar]} else {stop("outcomevar is empty")}
  if(timevar != ""){x[, "GROUPINGG"] <- x[, groupvar]} else {stop("groupingvar is empty")}
  if(timevar != ""){
    x[, "TIMEPOINT"] <- x[, timevar]
    x2 <- x %>% dplyr::group_by(GROUPINGG, TIMEPOINT) %>% 
      dplyr::summarise(Mean = round(mean(OUTCOMEE, na.rm = T), 2), 
                       SD = round(sd(OUTCOMEE, na.rm = T), 2), 
                       N = length(na.omit(OUTCOMEE)), 
                       Med = round(median(OUTCOMEE, na.rm = TRUE), 2), 
                       Min = round(min(OUTCOMEE, na.rm = TRUE), 2), 
                       Max = round(max(OUTCOMEE, na.rm = TRUE), 2))
  } else {
    x2 <- x %>% dplyr::group_by(GROUPINGG) %>% 
      dplyr::summarise(Mean = round(mean(OUTCOMEE, na.rm = T), 2), 
                       SD = round(sd(OUTCOMEE, na.rm = T), 2), 
                       N = length(na.omit(OUTCOMEE)), 
                       Med = round(median(OUTCOMEE, na.rm = TRUE), 2), 
                       Min = round(min(OUTCOMEE, na.rm = TRUE), 2), 
                       Max = round(max(OUTCOMEE, na.rm = TRUE), 2))
  }
  
  
  x3 <- as.data.frame(x2)
  return(x3)
}
