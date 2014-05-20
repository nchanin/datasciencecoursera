myfunction <- function() {
  x <- rnorm(100)
  mean(x)
}

second <- function(x) {
  x + rnorm(length(x))
}


pollutantmean <- function(directory, pollutant, id = 1:332) {
  options(digits=3)
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  specdata <- do.call(rbind, lapply(filenames, read.csv))
  mean(specdata[specdata$ID %in% id, c(pollutant)], na.rm=T)
  
}

complete <- function(directory, id = 1:332) {
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
  
  
  # https://stat.ethz.ch/pipermail/r-help/2008-April/159060.html
  #allData <- lapply(filenames, function(.file){
  #  dat<-read.csv(.file)  
  #  dat$File<-as.character(.file)
    
  #  dat    # return the dataframe
  #})
  # combine into a single dataframe
  #specdata <- do.call(rbind, allData) 
  
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  specdata <- do.call(rbind, lapply(filenames, read.csv))
  dataset <- specdata[specdata$ID %in% id, ]
  with(dataset[complete.cases(dataset),], aggregate((list(nops = ID)), list(ID = ID), length))[id, ]
  
}
