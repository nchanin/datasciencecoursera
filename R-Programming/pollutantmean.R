## Noah Chanin
## 2014-MAY-20

## pollutantmean takes in:
## a directory that has the pollutant data csv files
## a pollutant: either "sulfate" or "nitrate"
## the id integeters to examine
## pollutantean returns:
## the mean of selected pollutant values for the id set
## is returned, NA values are ignored

pollutantmean <- function(directory, pollutant, id = 1:332) {
  options(digits=4)
  ## filenames is a list of csv files in the directory
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  ## read all csv files into a dataframe
  specdata <- do.call(rbind, lapply(filenames, read.csv))
  ## calculate the mean of the selected pollutant for the given id
  ## igorning NA values
  mean(specdata[specdata$ID %in% id, c(pollutant)], na.rm=TRUE)
}

## test data
# source("pollutantmean.R")
# pollutantmean("specdata", "sulfate", 1:10)
# 
# ## [1] 4.064
# 
# pollutantmean("specdata", "nitrate", 70:72)
# 
# ## [1] 1.706
# 
# pollutantmean("specdata", "nitrate", 23)
# 
# ## [1] 1.281
