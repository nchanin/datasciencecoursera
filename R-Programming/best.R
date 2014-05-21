## Noah Chanin
## 2014-MAY-20

## best takes in:
## a state (two letter United States state abbreviation code)
## an outcome: one of "heart attack", "heart failure", or "pneumonia"
## best returns:
## the hospital with the lowest 30 day mortality rate for the give state and outcome

best <- function(state, outcome) {

  ## mapcol takes the user provided outcome and maps it to the associated 30 day mortality data
  mapcol = ""
  if (outcome == "heart attack")  mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  if (outcome == "heart failure") mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  if (outcome == "pneumonia")     mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if (mapcol == "") stop("invalid outcome")
  
  ## read in the csv file
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## make sure the user provided state is valid
  if(nrow(dat[dat$State==state, ]) == 0) stop("invalid state")

  ## filter by state
  df <- dat[dat$State==state,c("Hospital.Name", mapcol)]
  
  ## remove NA
  df <- df[complete.cases(df) & df[[2]] != "Not Available", ]
  
  ## make results numeric
  df <- df[order(as.numeric(df[,2]),df[,1]), ]
  
  ## and return results
  as.vector(df[1, c('Hospital.Name')])
  
}

## test data
# > source("best.R")
# > best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
# > best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
# > best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
# > best("MD", "pneumonia")
# [1] "GREATER BALTIMORE MEDICAL CENTER"
# > best("BB", "heart attack")
# Error in best("BB", "heart attack") : invalid state
# > best("NY", "hert attack")
# Error in best("NY", "hert attack") : invalid outcome
