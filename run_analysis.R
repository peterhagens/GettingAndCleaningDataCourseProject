# Course project for Getting and Cleaning Data 
library(dplyr)

# Merges the training and the test sets to create one data set.
dataset  <- read.table("test/X_test.txt",header=F)
dataset  <- cbind(dataset,read.table("test/Y_test.txt",header=F))
train  <- read.table("train/X_train.txt",header=F)
train  <- cbind(train,read.table("train/Y_train.txt",header=F))
dataset  <- rbind(train, dataset)

# Only the results with mean or standard deviation
# Read labels from features.txt file
labels  <- read.table("features.txt")
# filter out labels that are mean or std
lab  <- filter(labels, grepl("mean\\(\\)", V2) | grepl("std()", V2))

colnames(dataset)  <- c(1:562)
extractcols  <- lab[,1]
# add last column (for activity)
extractcols  <- c(extractcols,562)
# filter columns
tidyset  <- select(dataset, extractcols)

# tidy colnames into variable vars
vars  <- gsub("-","",lab[,2])
vars <- gsub("\\(","",vars)
vars  <- gsub("\\)","",vars)
# add row for activity id
vars  <- c(vars,"activity_id")
colnames(tidyset)  <- vars

# Uses descriptive activity names to name the activities in the data set
activity_labels  <- read.table("activity_labels.txt")
colnames(activity_labels)  <- c("activity_id", "activity")
merged  <- merge(tidyset, activity_labels, by.x="activity_id", by.y="activity_id",all=F)
# remove activity_id from dataset
merged  <- select(merged, 2:68)

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
trainSubjects  <- read.table("train/subject_train.txt",header=F)
testSubjects <- read.table("test/subject_test.txt",header=F)
subjects  <- rbind(trainSubjects, testSubjects)
colnames(subjects)  <- "subject"

# merge column to dataset
setwithsubjects  <- cbind(merged,subject = subjects)

# group by activity and subject
byact  <- group_by(setwithsubjects, activity, subject)
# calculate the mean
sum  <- summarise_each(byact, funs(mean))

# save result
write.table(sum, "tidyset.txt", row.name=F)