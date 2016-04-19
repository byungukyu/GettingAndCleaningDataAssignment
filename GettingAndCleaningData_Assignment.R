## Merge the training and the test sets to create one data set
#1. Read Files
features <- read.table('./features.txt',header=FALSE)
activityType <- read.table('./activity_labels.txt',header=FALSE)
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE)
xTrain <- read.table('./train/x_train.txt',header=FALSE)
yTrain <- read.table('./train/y_train.txt',header=FALSE)

#2. Assign column names to the data 
colnames(activityType) <- c('activityId','activityType')
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"

#3. Create the final training set 
trainingData <- cbind(yTrain,subjectTrain,xTrain)

#4. Read test data
subjectTest <- read.table('./test/subject_test.txt',header=FALSE)
xTest <- read.table('./test/x_test.txt',header=FALSE)
yTest <- read.table('./test/y_test.txt',header=FALSE)

#5. Assign column names to the test data
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2] 
colnames(yTest) <- "activityId"


#6. Create the final test set 
testData <- cbind(yTest,subjectTest,xTest)


#7. Combine training and test data to create a final data set
finalData <- rbind(trainingData,testData)

#8. Create a vector for the column names from the finalData
colNames <- colnames(finalData)

## Extract only the measurements on the mean and standard deviation for each measurement
#9. Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

#10. Subset finalData table based on the logicalVector to keep only desired columns
finalData <- finalData[logicalVector==TRUE]

#11. Use descriptive activity names to name the activities in the data set

## Use descriptive activity names to name the activities in the data set
#12. Merge the finalData set with the acitivityType table to include descriptive activity names
finalData <- merge(finalData,activityType,by='activityId',all.x=TRUE)

#13. Updating the colNames vector to include the new column names after merge
colNames <- colnames(finalData)

##Appropriately label the data set with descriptive activity names. 

#14. Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

#15. Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- colNames

##. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

#16. Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  <- finalData[,names(finalData) != 'activityType']

#17. Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData_Assignment_Result.txt',row.names=TRUE,sep='\t')
