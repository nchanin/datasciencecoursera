Programming Assignment for Getting and Cleaning Data
===================

This program reads in "X" training and testing data from the UCR HAR Datset.
The "X" data contains a total of 10299 measures for 561 features.
This "X" data is read into a dataframe called "df" that has 10299 rows and 561 features.

There is a UCE HAR Dataset file called "features.txt".
It has the name of the 561 features in it.
This is read in to a dataframe called "features".
The features with "mean" or "std" in the name are saved to a datafrane called "selected.features".

There is a UCE HAR Dataset file called "activity_labels.txt".
It is read into a dataframe called "activity_labels".
This maps the 5 different kinds of activities measured to a meaningful value, like "WALKING".

This program reads in "Y" training and testing data from the UCR HAR Datset.
The "Y" data contains an activity code for each of the 10299 measures
This "Y" data is read into a dataframe called "activity".

The columns in the measurements dataframe (df) are renamed with the lookup values in the features dataframe.
Then, the selected.features are selected from the df dataframe, and saved to the df dataframe.
This removed the columns that we aren't interested in.
At this point, the number of columns in the df dataframe is saved to "num_measures".
This means we will be able to refernce df[, 1:num_measures] in the future to select all of the measure columns.

Similar to "Y" activity data, there are 10299 subjects that come from the UCI HAR Dataset subject_train.txt and subject_test.txt.
This data is read into a dataframe called subject.


Now the Activity and Subject data is added to the df dataframe.
And then the activity_label data is merged in so that there is data like "WALKING" in the df instead of a code like 1.
After this is complete, the original Activity ID is no longer needed, so this column is removed from df.

At this point, we have all of the data we need, and we can start producing the tidy results.
This is done by getting a distinct set of Activity and Subject value from df.
These values are ordered so that the results will be ordered.
Then, the program loops through these values, extracting all measurements for each group of Activity and Subject codes.
This data is simply colMeans'ed together, and saved to an "answer" dataframe.
When the loop completes, and answer dataframe is saved to a file called "tidy.txt"
