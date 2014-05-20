complete <- function(directory, id = 1:332) {
  capture.output(directory, file ="complete.log")
  capture.output(id, file ="complete.log", append = TRUE)

  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  

  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  specdata <- do.call(rbind, lapply(filenames, read.csv))
  dataset <- specdata[specdata$ID %in% id, ]
  #dataset <- within(dataset, nobs <- !is.na(sulfate) & !is.na(nitrate))
  #within(dataset, nobs <- !is.na(sulfate) & !is.na(nitrate))
  #lapply(split(dataset, dataset$ID), function(X) sum(X$nobs))
  result <- with(dataset[complete.cases(dataset), ], aggregate((list(nobs = ID)), list(ID = ID), length))
  # rearrange by id
  # adding 0s back in is cheesy
  result2 <- merge(as.data.frame(id), result, by.x="id", by.y="ID", sort=FALSE, all.x=TRUE)
  result2[is.na(result2)] <- 0
  result2 <- merge(as.data.frame(id), result2, by.x="id", by.y="id", sort=FALSE, all.x=TRUE)
  
  result2

}


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
