# DATA SCIENCE SPECIALIZATION
# Getting and cleaning the data
# 
# Course Project
# run_analysis.R

#Libraries
library(data.table)
library(plyr)
    
#Unzip the original database 
unzip("getdata-projectfiles-UCI HAR Dataset.zip")

#Load the files into R
features <- read.table(".\\UCI HAR DATASET\\features.txt")
names <- c(as.character(features[,2]), "Activity", "Subject")
x_train <- read.table(".\\UCI HAR DATASET\\train\\X_train.txt")
y_train <- read.table(".\\UCI HAR DATASET\\train\\y_train.txt")
s_train <- read.table(".\\UCI HAR DATASET\\train\\subject_train.txt")
train <- data.table(cbind(x_train, y_train, s_train))
setnames(train, names) #Step 4
rm(x_train, y_train, s_train)

x_test <- read.table(".\\UCI HAR DATASET\\test\\X_test.txt")
y_test <- read.table(".\\UCI HAR DATASET\\test\\y_test.txt")
s_test <- read.table(".\\UCI HAR DATASET\\test\\subject_test.txt")
test <- data.table(cbind(x_test, y_test, s_test))
setnames(test, names) #Step 4
rm(x_test, y_test, s_test)
rm(features)

#Step1: Merges the training and the test sets to create one data set.
dataset <- rbind(train, test)
rm(train, test)

#Step2: Extracts only the measurements on the mean and standard deviation 
#for each measurement.
mean_variables <- names[grep("mean()", names, fixed = T)]
std_variables <- names[grep("std()", names, fixed = T)]
dataset <- subset(dataset, select = c(mean_variables, std_variables,"Activity", "Subject"))
rm(mean_variables, std_variables)

#Step3: Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table(".\\UCI HAR DATASET\\activity_labels.txt")
names(activity_labels) <- c("Activity", "ActivityLabel")
dataset <- join(dataset, activity_labels, by="Activity")
rm(activity_labels, names)

#Step4: Appropriately labels the data set with descriptive variable names.
    #Already done.

#Step5: From the data set in step 4, creates a second, independent tidy 
#data set with the average of each variable for each activity and each subject.
dataset$ActivitySubject <- paste(dataset$Activity, dataset$Subject, sep = ".")
dataset[,paste("mean",names(dataset)[1:66], sep = "_") := lapply(.SD, mean),
    by = ActivitySubject, .SDcols = 1:66]
brief_dataset <- unique(subset(dataset, select = c(69, 68, 71:136)))
dataset <- subset(dataset, select = c("ActivityLabel", "Subject", names(dataset)[1:66]))

#Write the tidy data into a txt file
write.table(brief_dataset, "brief_dataset.txt", row.names = FALSE, sep = "\\t")
write.table(dataset, "dataset.txt", row.names = FALSE, sep = "\\t")
    