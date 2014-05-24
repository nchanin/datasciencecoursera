## Noah Chanin
## 2014-MAY-20

## complete takes in:
## a directory that has the pollutant data csv files
## the id integers to examine
## complete returns:
## the mean of selected pollutant values for the id set
## is returned, NA values are ignored


complete <- function(directory, id = 1:332) {

  ## get the list of csv files
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  ## read all csv files into a dataframe
  specdata <- do.call(rbind, lapply(filenames, read.csv))
  ## filter out the "id" values of interest
  dataset <- specdata[specdata$ID %in% id, ]
  ## get the number of complete.cases by ID
  result <- with(dataset[complete.cases(dataset), ], aggregate((list(nobs = ID)), list(ID = ID), length))

  ## result has filtered out non-complete cases
  ## merge in the ids to add NA cases back in
  result2 <- merge(as.data.frame(id), result, by.x="id", by.y="ID", sort=FALSE, all.x=TRUE)
  ## set NA values to 0
  result2[is.na(result2)] <- 0
  ## return the data in the order of the user provided id data
  merge(as.data.frame(id), result2, by.x="id", by.y="id", sort=FALSE, all.x=TRUE)

}


## test data
# source("complete.R")
# complete("specdata", 1)
# 
# ##   id nobs
# ## 1  1  117
# 
# complete("specdata", c(2, 4, 8, 10, 12))
# 
# ##   id nobs
# ## 1  2 1041
# ## 2  4  474
# ## 3  8  192
# ## 4 10  148
# ## 5 12   96
# 
# complete("specdata", 30:25)
# 
# ##   id nobs
# ## 1 30  932
# ## 2 29  711
# ## 3 28  475
# ## 4 27  338
# ## 5 26  586
# ## 6 25  463
# 
# complete("specdata", 3)
# 
# ##   id nobs
# ## 1  3  243
