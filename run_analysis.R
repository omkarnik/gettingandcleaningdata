library(data.table)
library(dplyr)


######  1.Merges the training and the test sets to create one data set. ######
#Read supporting data
features <- read.table("UCI HAR Dataset/features.txt")
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Read test data
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Read train data
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#Merge test and train data
mergesubject <- rbind(subjecttrain, subjecttest)
mergeactivity <- rbind(activityTrain, activityTest)
mergefeatures <- rbind(featuresTrain, featuresTest)

#Naming the columns
colnames(mergefeatures) <- t(features[2])

#Merge final data
colnames(mergeactivity) <- "Activity"
colnames(mergesubject) <- "Subject"
mergefinal<- cbind(mergefeatures,mergeactivity,mergesubject)




###### 2.Extracts only the measurements on the mean and standard deviation for each measurement. ######
#Extract columns with mean and standard deviation
measurementswithMeanSTD <- grep(".*Mean.*|.*Std.*", names(mergefinal), ignore.case=TRUE)

#Adding activity and subject columns
columnsneeded <- c(measurementswithMeanSTD, 562, 563)
dim(mergefinal)

#Creating data extract with columnsneeded
dataextract <- mergefinal[,columnsneeded]
dim(dataextract)



###### 3.Uses descriptive activity names to name the activities in the data set ######
dataextract$Activity <- as.character(dataextract$Activity)
for (i in 1:6){
  dataextract$Activity[dataextract$Activity == i] <- as.character(activitylabels[i,2])
}
dataextract$Activity <- as.factor(dataextract$Activity)



###### 4.Appropriately labels the data set with descriptive variable names. ######
names(dataextract)
names(dataextract)<-gsub("Acc", "Accelerometer", names(dataextract))
names(dataextract)<-gsub("Gyro", "Gyroscope", names(dataextract))
names(dataextract)<-gsub("BodyBody", "Body", names(dataextract))
names(dataextract)<-gsub("Mag", "Magnitude", names(dataextract))
names(dataextract)<-gsub("^t", "Time", names(dataextract))
names(dataextract)<-gsub("^f", "Frequency", names(dataextract))
names(dataextract)<-gsub("tBody", "TimeBody", names(dataextract))
names(dataextract)<-gsub("-mean()", "Mean", names(dataextract), ignore.case = TRUE)
names(dataextract)<-gsub("-std()", "STD", names(dataextract), ignore.case = TRUE)
names(dataextract)<-gsub("-freq()", "Frequency", names(dataextract), ignore.case = TRUE)
names(dataextract)<-gsub("angle", "Angle", names(dataextract))
names(dataextract)<-gsub("gravity", "Gravity", names(dataextract))



###### 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. ######
dataextract$Subject <- as.factor(dataextract$Subject)
dataextract <- data.table(dataextract)


#Creating a tidy dataset
tidyData <- aggregate(. ~Subject + Activity, dataextract, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

