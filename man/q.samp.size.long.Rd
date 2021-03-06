% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/q.samp.size.long.R
\name{q.samp.size.long}
\alias{q.samp.size.long}
\title{Sample size for comparing means for longitudinal data}
\usage{
q.samp.size.long(ref.mean=NULL, comp.mean=NULL, baseline.sd=NULL, 
                endline.sd=NULL, dropout=0.2, arms=2, rho=0.5,
                sig.level = 0.05 , power = 0.8)
}
\arguments{
\item{ref.mean}{Mean for change in values within a treatment group}

\item{comp.mean}{Mean for change in values within the comparison group}

\item{baseline.sd}{Standard deviation for baseline values}

\item{endline.sd}{Standard deviation for end-of-study/follow-up values}

\item{dropout}{Dropout rate to be accounted for in sample size calculation. Default is 0.2}

\item{arms}{Number of arms in the planned study}

\item{rho}{Correlation coefficient between baseline and follow-up values within a treatment group. Default is 0.5}

\item{sig.level}{Significance level. Default is 0.05}

\item{power}{Power. Default is 0.8}
}
\value{
A list containing the mean difference, mean ratio, number of completers per arm, total sample size per arm, total sample size for the study and number of study arms
}
\description{
Calculate sample size for comparing mean changes for longitudinal data involving 2 time points based on \code{powerMediation::ssLongFull}
}
\examples{
ref.mean <- 57.2
comp.mean <- 22.8
baseline.sd <- 21.1
endline.sd <- 32.2
q.samp.size.long(ref.mean, comp.mean, baseline.sd, endline.sd, dropout=0.2)
}
\seealso{
powerMediation::ssLongFull
}
\author{
Abdul Malik Sulley <asulley@uwo.ca> May 8, 2020
}
