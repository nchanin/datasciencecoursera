# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive activity names. 
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

## just a wrapper around reading lookup data to avoid duplicate code
readmy.lookup <- function(files)
{
  read.table(files, colClasses = c('numeric', 'character'), comment.char = '')
}

## and a wrapper around reading in the main data fields again to aboid code redudancy
readmy.data <- function(files)
{
  df <- do.call(rbind, lapply(files, function(x) read.table(x, colClasses='numeric', comment.char='')))
}

## features.txt has a number from 1 to 561 and activities
## with names like tBodyAcc-mean()-X and tBodyAcc-std()-X
features <- readmy.lookup(c('UCI HAR Dataset/features.txt'))

## select out the features we are interested in:
## mean and std
selected.features <- features[with(features, grep('\\-(mean|std)\\(', V2, perl = TRUE)), c('V1')]

## the X_train.txt has 561 columns
## each column maps to one of the 561 features
## the train data has 2947 rows
## and the test data has 7352 rows
## so we are expecting 10299 rows or measurements for the 561 features

## read in these two files into a dataframe with a combined 10299 obs/measurements and 561 variables/features
df <- readmy.data(c('UCI HAR Dataset/train/X_train.txt', 'UCI HAR Dataset/test/X_test.txt'))

## activity laels is a simple lookup between the numeric activity label and its description
activity_labels <- readmy.lookup(c('UCI HAR Dataset/activity_labels.txt'))

## the y files have a combined 10299 activies, one for each measurement
activity <- readmy.data(c('UCI HAR Dataset/train/y_train.txt', 'UCI HAR Dataset/test/y_test.txt'))

## rename the columns in df with their associate description
colnames(df) <- features$V2

## remove the columns in df that aren't mean or std
df <- df[selected.features]

## get the number of resulting columns in df
num_measures <- length(names(df))

## there are 10299 subjects, one for each measurement similar to activity
subject <- readmy.data(c('UCI HAR Dataset/train/subject_train.txt', 'UCI HAR Dataset/test/subject_test.txt'))
 
## add subject and activity to the dataframe
df[c('Subject', 'V1')] <- c(subject$V1, activity$V1)

## add the activity label descriptions
df <- merge(df, activity_labels) #, by.x = "V1", by.y = "V1"

## rename merged in activity label lookup value
colnames(df)[colnames(df)=="V2"] <- "Activity"
## remove the old V1 columns
df <- df[,!(colnames(df) %in% c('V1'))]

## This doesn't seem like the most elegant way to get to the answer
## on the other hand, it seems fairly clear and fast enough
u <- unique(df[, c('Activity', 'Subject')])
groups <- u[with(u, order(Activity, Subject)), ]
num_groups <- nrow(groups)
answer <-  cbind(df[0,c('Activity', 'Subject')], df[0,1:num_measures])
for(i in 1:num_groups){
  chkact  <- groups[i, c('Activity')]
  chksub  <- groups[i, c('Subject')]

  mygroup <- df[df$Subject==chksub & df$Activity==chkact,1:num_measures]
  result  <- colMeans(mygroup)

  answer[i,c('Activity')] <- chkact
  answer[i,c('Subject')]  <- chksub
  answer[i,3:ncol(row)]   <- data.frame(rbind(result))
  
}

## save ordered results to file
write.table(answer, "tidy.txt")
