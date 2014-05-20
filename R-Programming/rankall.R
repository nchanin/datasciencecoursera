rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  mapcol = ""
  if (outcome == "heart attack")  mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  if (outcome == "heart failure") mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  if (outcome == "pneumonia")     mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if (mapcol == "") stop("invalid outcome")
  
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # filter by state?uns
  df <- dat[,c("Hospital.Name", "State", mapcol)]
  # remove NA
  df <- df[!is.na(df[[3]]) & df[[3]] != "Not Available", ]
  
  nth <- function (x){
    #cat(x[1, c('State')])
    state <- x[1, c('State')]
    # x should be a set of all state values
    x <- x[order(as.numeric(x[,3]),x[,1]), ]
    #class(x)
    #str(x)
    if (num == "best")       res <- x[1, c('Hospital.Name', 'State')]
    else if (num == "worst") res <- tail(x[, c('Hospital.Name', 'State')], 1)
    else                     res <- x[num, c('Hospital.Name', 'State')]
    res$State[is.na(res$State)] <- state
    res
  }
  
  answer <- do.call(rbind, by(df, df[,"State"], nth))
  names(answer)[names(answer)=="Hospital.Name"] <- "hospital"
  names(answer)[names(answer)=="State"] <- "state"
  answer
  
  #  
  
}


# > source("rankall.R")
# > head(rankall("heart attack", 20), 10)
# hospital state
# AK <NA> AK
# AL D W MCMILLAN MEMORIAL HOSPITAL AL
# AR ARKANSAS METHODIST MEDICAL CENTER AR
# AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
# CA SHERMAN OAKS HOSPITAL CA
# CO SKY RIDGE MEDICAL CENTER CO
# CT MIDSTATE MEDICAL CENTER CT
# DC <NA> DC
# DE <NA> DE
# FL SOUTH FLORIDA BAPTIST HOSPITAL FL
# > tail(rankall("pneumonia", "worst"), 3)
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY
# > tail(rankall("heart failure"), 10)
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY
