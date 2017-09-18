#Getting and Cleaning Data Week 4 Project

# Assumptions: 
# This run_analysis.R script can be run as long as
# the Samsung data is unzipped and the working directory is set to the folder
# inside the zip. All loaded packages are pre-installed.

#Load packages
library(tidyr)
library(dplyr)
library(reshape2)

#Extract activity labels
activity_labels <- readLines("activity_labels.txt")
activity_labels <- separate(data = data.frame(activity_labels),col = "activity_labels",sep = " ",into =c("activityNumber","activityLabel"))
activity_labels[2]<-tolower(activity_labels[[2]])

#Extract features
features <- readLines("features.txt")
features <- c(substring(grep("^[0-9]{1} [a-z]",features,value = TRUE),3),
              substring(grep("^[0-9]{2} [a-z]",features,value = TRUE),4),
              substring(grep("^[0-9]{3} [a-z]",features,value = TRUE),5))

# 1. MERGE TRAINING SET & TEST SET

train_subject_ID <- read.table("train/subject_train.txt")         #Extract training subject identifiers
train_features <- read.table("train/X_train.txt")                #Extract training variables
train_labels <- read.table("train/y_train.txt")                   #Extract training labels
names(train_features) <- features
training_data <- cbind(rep("training",nrow(train_features)), train_subject_ID, train_labels, train_features)
names(training_data)[1:3] <- c("DataSet","subjectID","outcomeLabels")

test_subject_ID <- read.table("test/subject_test.txt") #Extract test subject identifiers
test_features<- read.table("test/X_test.txt")             #Extract test dataset
test_labels <- read.table("test/y_test.txt")             #Extract test labels
names(test_features) <- features
test_data <- cbind(rep("test",nrow(test_features)), test_subject_ID, test_labels, test_features)
names(test_data)[1:3] <- c("DataSet","subjectID","outcomeLabels")

combined_data <- rbind(training_data,test_data)

# 2. USE DESCRIPTIVE ACTIVITY NAME
combined_data <- merge(x = activity_labels, y=combined_data, by.y = "outcomeLabels", by.x = "activityNumber")
combined_data <- combined_data[,-1]
names(combined_data)[1] <- "activityName"
combined_data <- combined_data[,c(3,2,1,4:ncol(combined_data))]

# 3. EXTRACT MEAN & STANDARD DEVIATION MEASUREMENTS

mean_column_index <- grep("mean[()]",names(combined_data))
standard_deviation_column_index <- grep("std",names(combined_data))
final_data<- combined_data[,c(1:3,mean_column_index,standard_deviation_column_index)]
final_data<- tbl_df(final_data)

# 4. LABEL VARIABLES IN DATASETS WITH DESCRIPTIVE NAMES
names(final_data)<-gsub('^([ft])', '\\1\\.', names(final_data))
names(final_data)[4:36]<-paste0(names(final_data)[4:36],".Mean")
names(final_data)[37:69]<-paste0(names(final_data)[37:69],".Std")
names(final_data)<-gsub("(-std\\(\\)|-mean\\(\\))","",names(final_data))
names(final_data)<-gsub("-",".",names(final_data))
names(final_data)<-gsub("BodyBody","Body",names(final_data))
names(final_data)<-gsub("Mag","Magnitude",names(final_data))
names(final_data)<-gsub("AccJerk","LinearJerk",names(final_data))
names(final_data)<-gsub("GyroJerk","AngularJerk",names(final_data))
names(final_data)<-gsub("Acc","LinearAcceleration",names(final_data))
names(final_data)<-gsub("Gyro","AngularVelocity",names(final_data))
final_data <- final_data %>% select(-observationPurpose)

# 5. CALCULATE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT
meltdata<-melt(final_data, id.vars = c("subjectID","activityName"))
meltdata<- tbl_df(meltdata)
tidydata<- meltdata %>% group_by(subjectID,activityName,variable) %>% summarize(average=mean(value))
names(tidydata)[3] <- "feature"
write.table(tidydata,file = "tidy.txt",row.names = FALSE)