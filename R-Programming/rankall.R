## Noah Chanin
## 2014-MAY-20

## rankall takes in:
## an outcome: "heart attack", "heart failure", or "pneumonia"
## a num: "best", "worst", or a number
## rankall returns:
## a dataframe with the state and hospital 
## where hospital is the "num"th best hospital in the state

rankall <- function(outcome, num = "best") {

  ## mapcol maps the user provided outcome to the dataset outcome
  mapcol = ""
  if (outcome == "heart attack")  mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  if (outcome == "heart failure") mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  if (outcome == "pneumonia")     mapcol = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if (mapcol == "") stop("invalid outcome")
  
  ## read in the dataset
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## extract out the columns of interest
  df <- dat[,c("Hospital.Name", "State", mapcol)]
  
  # remove NA data
  df <- df[!is.na(df[[3]]) & df[[3]] != "Not Available", ]
  
  ## nth takes in:
  ## x, a dataframe that has all the records for a state
  ## nth returns:
  ## the "num"th best row from the state
  
  nth <- function (x){
    ## order up data by outcome, and then hospital name
    x <- x[order(as.numeric(x[,3]),x[,1]), ]

    ## select the numth best record for this state
    if (num == "best")       res <- x[1, c('Hospital.Name', 'State')]
    else if (num == "worst") res <- tail(x[, c('Hospital.Name', 'State')], 1)
    else                     res <- x[num, c('Hospital.Name', 'State')]
    
    ## if there are no outcomes at the numth position, replace NA state code with vaid state code
    res$State[is.na(res$State)] <- x[1, c('State')]
    res
  }
  
  ## for each State in dataframe, extract it, apply "nth" to it, put results in answer
  answer <- do.call(rbind, by(df, df[,"State"], nth))
  
  ## rename answer columns
  names(answer)[match(c('Hospital.Name', 'State'),names(answer))] <- c('hospital', 'state')

  ## and return results
  answer
}


## test data
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
