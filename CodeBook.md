# CodeBook.md

# Final Project for Cleaning Data

# Step 1: Download zip file from:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# unzip into working directory

# Step 2:
# Load needed libraries

library(readr) #needed for read_table
library(plyr) # needed for mutate
library(stringr) # needed for grep
library(dplyr) # needed for bind_rows

# Step 3:
# Load data
# Note that data in inertial folders are not needed
# col_names = FALSE is needed as colnames not stored in first row

subject_test <- read_table("./data/UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)
X_test <- read_table("./data/UCI HAR Dataset/test/X_test.txt", col_names = FALSE)
Y_test <- read_table("./data/UCI HAR Dataset/test/Y_test.txt", col_names = FALSE)
subject_train <- read_table("./data/UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)
X_train <- read_table("./data/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)
Y_train <- read_table("./data/UCI HAR Dataset/train/Y_train.txt", col_names = FALSE)
features <- read_table("./data/UCI HAR Dataset/features.txt", col_names = FALSE)
activity_labels <- read_table("./data/UCI HAR Dataset/activity_labels.txt", col_names = FALSE)

# Step 4:
# subset features dataframe using grep
# select only those features where "mean" or "std" is found
# this results in a new df with 86 columns
# must account for possible spaces before and after, and all combinations of upper and lower case letters
# * does not seem to be needed, results are the same with or without using it at the front, middle or back
# this accomplishes #2 "Extracts only the measurements on the mean and standard deviation for each measurement"

features2 <- features[(grep("[Mm][Ee][Aa][Mn]|[Ss][Tt][Dd]", features$X2)),]

# Step 5:
# Subset testdata and traindata dataframes, selecting only columns identified in features2 (ie. mean, std)
# successfully extracts 86 variable as expected

X_test2 <- X_test[, features2$X1]
X_train2 <- X_train[, features2$X1]

# Step 6:
# using colnames() function to link description in col2 of features2 dataframe to the test and train dataframes
# this accomplishes #4 "Appropriately labels the data set with descriptive variable names"

colnames(X_test2) = features2$X2
colnames(X_train2) = features2$X2

# Step 7:
# merge X_test (the data) with Y_test (activity #) and subject_test (subject #)
# merge x_train, y_train and subject_train similarly

testdata <- mutate(X_test2, activity = Y_test$X1, subject = subject_test$X1)
traindata <- mutate(X_train2, activity = Y_train$X1, subject = subject_train$X1)

# Step 8:
# rename column headings for activity_labels dataframe to match "testdata" & "traindata" dataframes
# so we can use the join() function to add activity description

colnames(activity_labels) = c("activity", "activity_description")

# Step 9:
# add better description for activity columns, use join() to link to activity_labels
# this accomplishes #3 "Uses descriptive activity names to name the activities in the data set"

testdata2 <- join(testdata, activity_labels, by = "activity", type = "inner")
traindata2 <- join(traindata, activity_labels, by = "activity", type = "inner")

Step 10:
# merge training and test datasets, phase one
# first need to create a column in each called "set" to specific "train" or "test"

testdata3 <- mutate(testdata2, set = "test")
traindata3 <- mutate(traindata2, set = "train")

Step 11:
# merge training and test datasets, phase two
# append testdata3 to traindata3
# join wants to merge them column wise, so need to use bind_rows
# this accomplishes #1 "Merges the training and the test sets to create one data set"

merge_data <- bind_rows(testdata3, traindata3)

# Step 12:
# final table has average of each subject\activity combo for all variables
# Use groupby(), summarize() and pipeline
# Include "set" and "activity_description" in groupby() to have all the information

final_data <- merge_data %>%
  group_by(subject, set, activity, activity_description) %>%
  summarise(across("tBodyAcc-mean()-X":"angle(Z,gravityMean)", mean))

# Step 13:  
# QAQC on the mean calculation in Step 12:  from the merge_data, extract col1, subject 5, activity 5
# manually calculate mean =  0.2825444 
# compare to that combination in the final_data = 0.2825444, and it matches

QAQC <- merge_data[, (c(1, 87, 88))]
QAQC2 <- subset(QAQC, subject == 5)
QAQC3 <- subset(QAQC2, activity == 5)
mean(QAQC3$`tBodyAcc-mean()-X`)

# Step 14
# save final file as .txt with write.table() using row.name=FALSE
# this accomplishes the final step "From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject."



