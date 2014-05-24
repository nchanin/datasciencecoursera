## Noah Chanin
## 2014-MAY-20

## rankhospital takes in:
## a state (United States two character state abbreviation)
## an outcome: "heart attack", "heart failure", or "pneumonia"
## a num: either "best", "worst", or a number indicating the nth best
## rankhospital returns:
## a character vector with the "num"th best hospital for that outcome in the state

rankhospital <- function(state, outcome, num = "best") {

  ## mapcol maps the user provided outcome to the dataset outcome  
  mapcol = ""
  if (outcome == "heart attack")  mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  if (outcome == "heart failure") mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  if (outcome == "pneumonia")     mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if (mapcol == "") stop("invalid outcome")

  ## read the dataset
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(nrow(dat[dat$State==state, ]) == 0) stop("invalid state")
  
  ## filter by state
  df <- dat[dat$State==state,c("Hospital.Name", mapcol)]
  
  ## remove NA
  df <- df[complete.cases(df) & df[[2]] != "Not Available", ]
  
  ## make results numeric
  df <- df[order(as.numeric(df[,2]),df[,1]), ]
  
  ## handle "best", "worst", or num
  if (num == "best")   as.vector(df[1, c('Hospital.Name')])
  else if (num == "worst")  as.vector(tail(df[, c('Hospital.Name')], 1))
  else as.vector(df[num, c('Hospital.Name')])
  
}

## test data
# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
