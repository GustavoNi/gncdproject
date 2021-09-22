## Data source
# Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier
# Parra and Jorge L. Reyes-Ortiz. Human Activity
# Recognition on Smartphones using a Multiclass
# Hardware-Friendly Support Vector Machine. International
# Workshop of Ambient Assisted Living (IWAAL 2012).
# Vitoria-Gasteiz, Spain. Dec 2012

## Packages
library(plyr)
library(dplyr)

## Directory and files
if(!file.exists("./w4project")){if(!file.exists("./UCI HAR Dataset")){dir.create("./w4project")}}
if(file.exists("./w4project")){setwd("./w4project")}
w4purl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./w4datasets.zip")){download.file(w4purl, "./w4datasets.zip", method="curl")}
content<-list.files(path=getwd(), pattern= "*.zip", full.names = TRUE)
ldply(.data=content, .fun = unzip, exdir =getwd())
pathuci<-file.path(getwd(), "/UCI HAR Dataset")
pathuci

## Read / Load data
files<-list.files(path=pathuci, full.names = TRUE, recursive=TRUE)
files
train_set<-read.table(files[27], header = FALSE)
train_labs<-read.table(files[28], header = FALSE)
test_set<-read.table(files[15], header = FALSE)
test_labs<-read.table(files[16], header = FALSE)
sub_test<-read.table(files[14], header = FALSE)
sub_train<-read.table(files[26], header = FALSE)
act_labs<-read.table(files[1], header = FALSE)
feats<-read.table(files[2], header = FALSE)


## looking data properties from training:##
str(train_set)
str(train_labs)
str(sub_train)

## looking data properties from testing:##
str(test_set)
str(test_labs)
str(sub_test)

## other data properties:##
str(act_labs)
str(feats)

#0 Label variables:

names(sub_test) = c("subject")
names(sub_train) = c("subject")
x<-as.list(feats$V2)
names(test_set) = x
names(train_set) = x
colnames(act_labs)[2]<-"activity"   ## check step 2


#1 Merges the training and the test sets 
#  to create one data set.

training_ds<-cbind(sub_train, cbind(train_set, train_labs))
test_ds<-cbind(sub_test, cbind(test_set, test_labs))
joined_ds<-rbind(test_ds, training_ds)

#2 Uses descriptive activity names to name the
#  activities in the data set
joined_ds<-merge(joined_ds, act_labs, by.x=563, by.y = 1)
joined_ds<-joined_ds[2:564]

#3 Extracts only the measurements on the
#  mean and standard deviation for each measurement.
extracted_ds<- select(joined_ds, "subject", contains("mean")|contains("std"), "activity")
str(extracted_ds)

#4 Appropriately labels the data
#  set with descriptive variable names. 
 ### prefixes described in "features_info.txt
 ### prefix t = time_
 ### Acc = acceleration_
 ### Body = body_
 ### Gravity = gravity_
 ### Gyro = gyro_
 ### Jerk = jerk_
 ### Mag = magnitude_
 ### f = frecuency_

names(joined_ds) = gsub("^t", "time_", names(joined_ds))
names(joined_ds) = gsub("^f", "frecuency_", names(joined_ds))
names(joined_ds) = gsub("Acc", "acceleration_", names(joined_ds))
names(joined_ds) = gsub("Body", "body_", names(joined_ds))
names(joined_ds) = gsub("body_body_", "body_", names(joined_ds))
names(joined_ds) = gsub("Gravity", "gravity_", names(joined_ds))
names(joined_ds) = gsub("Gyro", "gyro_", names(joined_ds))
names(joined_ds) = gsub("Jerk", "jerk_", names(joined_ds))
names(joined_ds) = gsub("Mag", "magnitude_", names(joined_ds))
names(joined_ds) = gsub("Gyro", "gyro_", names(joined_ds))
str(joined_ds)


#5 From the data set in step 4, creates a second,
#  independent tidy data set with the average of
#  each variable for each activity and each subject.

indy_tds<-aggregate(joined_ds[, 2:562],list(c(joined_ds$subject), joined_ds$activity), mean)
colnames(indy_tds)[1]<-"subject"
colnames(indy_tds)[2]<-"activity"
head(str(indy_tds))

## DATA SET FILE CREATION:
write.table(joined_ds, file="UCI HAR Dataset/tidy_data.txt", row.names=FALSE)
