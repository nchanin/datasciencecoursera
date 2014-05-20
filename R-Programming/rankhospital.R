rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  mapcol = ""
  if (outcome == "heart attack")  mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  if (outcome == "heart failure") mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  if (outcome == "pneumonia")     mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if (mapcol == "") stop("invalid outcome")
  
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(nrow(dat[dat$State==state, ]) == 0) stop("invalid state")
  
  # filter by state
  df <- dat[dat$State==state,c("Hospital.Name", mapcol)]
  # remove NA
  df <- df[complete.cases(df) & df[[2]] != "Not Available", ]
  
  # make results numeric
  #df[, c(mapcol)] < as.numeric(df[, c(mapcol)])
  df <- df[order(as.numeric(df[,2]),df[,1]), ]
  
  if (num == "best")   as.vector(df[1, c('Hospital.Name')])
  else if (num == "worst")  as.vector(tail(df[, c('Hospital.Name')], 1))
  else as.vector(df[num, c('Hospital.Name')])
  
}

# > source("rankhospital.R")
# > rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
# > rankhospital("MD", "heart attack", "worst")
# [1] "HARFORD MEMORIAL HOSPITAL"
# > rankhospital("MN", "heart attack", 5000)
# [1] NA
