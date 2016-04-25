## set the path to the file containing data
setwd("d:/coursera/getting_and_cleaning/final_project/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")


## read all the features
feature_labels <- read.table("features.txt")[[2]]
feature_labels <- as.character(feature_labels)

## extract the mean and std features
feature_wanted <- grep(".*mean.*|.*std.*", feature_labels)
feature_labels <- feature_labels[feature_wanted]
feature_labels <- gsub("[()]", "", feature_labels)
feature_labels <- gsub("-mean-", "Mean", feature_labels)
feature_labels <- gsub("-std-", "Std", feature_labels)

## read the activity labels
activity_labels <- read.table("activity_labels.txt")[[2]]
activity_labels <- as.character(activity_labels)

## Read the train data and the test data
train_data <- read.table("train/X_train.txt")
test_data <- read.table("test/x_test.txt")

## extract the train and test data for wanted features
train_data_wanted <- train_data[, feature_wanted]
test_data_wanted <- test_data[, feature_wanted]

## read the train activity and subject
train_activity <- read.table("train/y_train.txt")
train_subject <- read.table("train/subject_train.txt")

## read the test activity and subject
test_activity <- read.table("test/y_test.txt")
test_subject <- read.table("test/subject_test.txt")

## combine the subject data and activity data with the train and test data respectively
train_data_wanted <- cbind(train_subject, train_activity, train_data_wanted)
test_data_wanted <- cbind(test_subject, test_activity, test_data_wanted)

## merge the test wanted and train wanted
all_data <- rbind(train_data_wanted, test_data_wanted)
colnames(all_data) <- c("subject", "activity", feature_labels)

## use descriptive names to name the activity 
activity <- all_data[[2]]
activity_des <- activity_labels[activity]
library(dplyr)
all_data <- mutate(all_data, activity = activity_des)

## group the all_data by subject and activity
group_activity <- split(all_data, all_data$activity)
group_subject <- split(all_data, all_data$subject)
dataframe_mean <- function(x) {
    n <- dim(x)[2]
    y <- x[, 3 : n]
    vec <- apply(y, 2, mean)
    return(vec)
}
mean_activity <- sapply(group_activity, dataframe_mean)
mean_activity <- t(mean_activity)
mean_activity <- data.frame(mean_activity)
mean_subject <- sapply(group_subject, dataframe_mean)
mean_subject <- t(mean_subject)
mean_subject <- data.frame(mean_subject)
tidy_data <- rbind(mean_subject, mean_activity)


