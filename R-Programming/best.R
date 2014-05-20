best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
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
  
  as.vector(df[1, c('Hospital.Name')])
  
}


# 2 Finding the best hospital in a state
# Write a function called best
# that take two arguments: the 2-character abbreviated name of a state and an outcome name
# The function reads the outcome-of-care-measures.csv
# and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
# in that state.
# The hospital name is the name provided in the Hospital.Name variable.
# The outcomes can be one of \heart attack", \heart failure", or \pneumonia". 
# Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.
# Handling ties
# . If there is a tie for the best hospital for a given outcome, then the hospital names should
# be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e. if hospitals \b", \c",
# and \f" are tied for best, then hospital \b" should be returned).
# The function should use the following template.
# best <- function(state, outcome) {
# ## Read outcome data
# ## Check that state and outcome are valid
# ## Return hospital name in that state with lowest 30-day death
# ## rate
# }
# The function should check the validity of its arguments. If an invalid
# state
# value is passed to
# best
# , the
# function should throw an error via the
# stop
# function with the exact message \invalid state". If an invalid
# outcome
# value is passed to
# best
# , the function should throw an error via the
# stop
# function with the exact
# message \invalid outcome".
# Here is some sample output from the function.
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
# >