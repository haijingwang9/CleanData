library(dplyr)
library(stringr)


## step 4: Appropriately labels the data set with descriptive variable names
# get feature names from features.txt and convert to valid names
features <- read.table("UCI HAR Dataset/features.txt")
flist <- features[, 2]
flist1 <- make.names(flist, unique = TRUE)

## step 1: Merges the training and the test sets to create one data set.

# read training data and assign column names from feature list
data_train <- read.table("UCI HAR Dataset/train/X_train.txt")
names(data_train) <- flist1
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
names(subject_train) <- c("Subject")
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")
names(activity_train) <- c("Activity")
# bind by columns, subject, activity and 561 features
train_set <- bind_cols(subject_train, activity_train, data_train)

# read test data
data_test <- read.table("UCI HAR Dataset/test/X_test.txt")
names(data_test) <- flist1
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
names(subject_test) <- c("Subject")
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")
names(activity_test) <- c("Activity")
test_set <- bind_cols(subject_test, activity_test, data_test)

# Combine train data and test data
all_set <- bind_rows(train_set, test_set)

## step 2: Extracts only the measurements of the mean and standard deviation
##         for each measurement

mean_std <- select(all_set, contains(".mean") | contains(".std"))

## step 3: Uses descriptive activity names to 
##         name the activities in the data set

all_set <- mutate(all_set, Activity=as.character(Activity))
trans_actvity <- function(x){
  y <- sub("1", "Walking", x)
  y <- sub("2", "WalkingUp", y)
  y <- sub("3", "WalkingDown", y)
  y <- sub("4", "Sitting", y)
  y <- sub("5", "Standing", y)
  y <- sub("6", "Laying", y)
  y
}
all_set <- mutate(all_set, Activity = trans_actvity(Activity))

## step 5: From the data set in step 4, creates a second, independent tidy 
##         data set with the average of each variable for each activity 
##         and each subject.

by_subject_activity <- group_by(all_set, Subject, Activity)
tidy_avg <- summarise_each(by_subject_activity, funs(mean))

## write.table(tidy_avg, "tidaydataset.txt", row.names=FALSE)

tiday_avg

