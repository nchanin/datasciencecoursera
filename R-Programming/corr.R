## Noah Chanin
## 2014-MAY-20

## corr takes in:
## a directory that has the pollutant data csv files
## a threshold that indicates the number of completed cases required
## corr returns:
## the correlaton between sulfate and nitrate data for ID sets 
## where the number of completed cases is over the threshold

corr <- function(directory, threshold = 0) {
  ## set the precision to match samples
  options(digits=4)
  ## get the list of csv files
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  ## read in all csv files to a dataframe
  specdata <- do.call(rbind, lapply(filenames, read.csv))

  ## all records where complete.cases is true
  completed = specdata[complete.cases(specdata), ]
  ## for all completed cases, calculate the number of observations by ID
  ncos <- with(completed, aggregate((list(nops = ID)), list(ID = ID), length))
  ## get all of the completed observations where the threshold is over the limit
  result = merge(x = completed, y = ncos[ncos$nops > threshold, ], by.X = "ID", by.y = "ID")
  ## extract the columns of interest
  work <- result[,c('ID', 'sulfate', 'nitrate')]
  ## apply the cor function for each ID set and return the results as a vector
  as.vector(sapply(split(work, work$ID), function(X) cor(X$sulfate, X$nitrate, use="complete.obs"), simplify = TRUE))
}


## test data
# source("corr.R")
# cr <- corr("specdata", 150)
# head(cr)
# 
# ## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
# 
# summary(cr)
# 
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
# 
# cr <- corr("specdata", 400)
# head(cr)
# 
# ## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
# 
# summary(cr)
# 
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
# 
# cr <- corr("specdata", 5000)
# summary(cr)
# 
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## 
# 
# length(cr)
# 
# ## [1] 0
# 
# cr <- corr("specdata")
# summary(cr)
# 
# ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# ## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
# 
# length(cr)
# 
# ## [1] 323



