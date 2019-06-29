#Libraries 
library(plyr)
library(dplyr)
library(stringr)


#Downloading the dataset 

filename <- "Coursera_DS3_Final.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


#Assigning all the dataframes 
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Combining data

#adding the subjects, activity code, and results for test and trained data
test_data <- bind_cols(x_test, y_test, subject_test)
test_data <- mutate(test_data, set_of_participants = "test_group" )
test_data <- test_data %>% select(set_of_participants, subject, code, everything())
train_data <- bind_cols(x_train, y_train, subject_train)
train_data <- mutate(train_data, set_of_participants = "trained_group" )
train_data <- train_data %>% select(set_of_participants, subject, code, everything())

#Combining test and train 
combined_data <- bind_rows(test_data, train_data, .id = NULL)

#Renaming the variable names   The variable names should be readable to non-domain experts. Example: transform tBodyAcc.mean...X to something like timeBodyAccelerometerXaxisMean - something like this.

combined_data <- combined_data %>% rename_at(vars(contains("Acc")), 
                                             funs(str_replace(., "Acc", "_Accelerometer_")))
combined_data <- combined_data %>% rename_at(vars(contains("Jerk")), 
                                             funs(str_replace(., "jerk", "Jerk_signals_")))
combined_data <- combined_data %>% rename_at(vars(contains("Gyro")), 
                                             funs(str_replace(., "Gyro", "_Gyroscope_")))
combined_data <- combined_data %>% rename_at(vars(contains("Mag")), 
                                             funs(str_replace(., "Mag", "Magnitude_")))
combined_data <- combined_data %>% rename_at(vars(starts_with("f")), 
                                             funs(str_replace(., "f", "fourier_")))
combined_data <- combined_data %>% rename_at(vars(ends_with("X")), 
                                             funs(str_replace(., "X", "XAxis")))
combined_data <- combined_data %>% rename_at(vars(ends_with("Y")), 
                                             funs(str_replace(., "Y", "YAxis")))
combined_data <- combined_data %>% rename_at(vars(ends_with("Z")), 
                                             funs(str_replace(., "Z", "XAxis")))
combined_data <- combined_data[, !duplicated(colnames(combined_data))]

combined_data <- rename(combined_data, activity = code)


#Altering the factor levels for activities 
combined_data$activity = as.factor(as.character(combined_data$activity))
levels(combined_data$activity) <- c("walking","walking_upstairs","walking_downstairs","sitting","standing","laying")

#extract the mean and standard deviation and save them into a new data set , here I took out meanFreq because it's a weighted average and gives inaccurate results. 
means_stds <- select(combined_data, set_of_participants, subject, activity, matches("mean"), matches("std"), -(matches("meanFreq")))

#Create new tidy data set
summary <- means_stds[,2:50] %>% group_by(subject, activity) %>% summarize_all(funs(mean))

