
setwd("C:/Users/Hiroshi Ikeda/Desktop/Coursera/3_Getting and Cleaning Data/Project")

library(plyr)
library(tidyr)

###1.Merges the training and the test sets to create one data set###

##Download dataset
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "dataset.zip")
download.file(url, f)
unzip("dataset.zip")

##load activity_labels and features
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")
activitylabels$V2 <- as.character(activitylabels$V2)
features$V2 <- as.character(features$V2)

#name columns of activitylabels
colnames(activitylabels)  = c('activity','activityType')

##load train data
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt")
Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
Ytrain <- read.table("UCI HAR Dataset/train/Y_train.txt")

##name columns of train data
colnames(subjecttrain) <- "subjectID"
colnames(Ytrain) <- "activity"
colnames(Xtrain) <- features[,2]

##merge train data
trainingdata <- cbind(Ytrain, subjecttrain, Xtrain)

##load test data
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt")
Xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
Ytest <- read.table("UCI HAR Dataset/test/Y_test.txt")

##name columns of test data
colnames(subjecttest) <- "subjectID"
colnames(Ytest) <- "activity"
colnames(Xtest) <- features[,2]

##merge test data
testdata <- cbind(Ytest, subjecttest, Xtest)

##merge training and test data
Alldata <- rbind(trainingdata, testdata)


###2.Extracts only the measurements on the mean and standard deviation for each measurement###
colnames <- colnames(Alldata)
Alldata_mean_std <- Alldata[grepl("activity|subjectID|.*mean.*|.*std.*", colnames)]

###3.Uses descriptive activity names to name the activities in the data set###
Alldata_mean_std <- merge(Alldata_mean_std, activitylabels, by = "activity", all = TRUE)

###4.Appropriately labels the data set with descriptive variable names###
colnames <- colnames(Alldata_mean_std)

colnames <- gsub('-mean', 'Mean', colnames)
colnames <- gsub('-std', 'Stdv', colnames)
colnames <- gsub('[()]', '', colnames)
colnames <- gsub('-', '', colnames)
colnames

colnames(Alldata_mean_std) <- colnames

###5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject###
split <- list(activity = Alldata_mean_std$activity, subjectID = Alldata_mean_std$subjectID)
Alldata_grouped_mean <- aggregate(Alldata_mean_std, by = split,  FUN = mean)
Alldata_grouped_mean <- Alldata_grouped_mean[,3:83]

#create key for grouped mean data using activity and subjectID
Alldata_grouped_mean <- Alldata_grouped_mean %>% mutate(activitykey = as.character(Alldata_grouped_mean$activity)) %>% mutate(subjectIDkey = as.character(Alldata_grouped_mean$subjectID)) %>% mutate(key = paste(activitykey,subjectIDkey, sep =""))

#create key for tidy data using activity and subjectID
TidyData <- Alldata_mean_std %>% mutate(activitykey = as.character(Alldata_mean_std$activity)) %>% mutate(subjectIDkey = as.character(Alldata_mean_std$subjectID)) %>% mutate(key = paste(activitykey,subjectIDkey, sep =""))

#merge above two data by key
TidyData <- merge(TidyData, Alldata_grouped_mean, by = "key", all = TRUE) 
TidyData <- TidyData[,86:166]

colnames <- colnames(TidyData)
colnames <- gsub('.y', '', colnames)
colnames <- gsub('activi', 'activity', colnames)
colnames(TidyData) <- colnames

#export TidyData
write.table(TidyData, './TidyData.txt',row.names=TRUE,sep='\t');