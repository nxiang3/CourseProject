library(dplyr)
#------------------------------------------------
# 1. Merge the training and the test sets to create one data set.

##step 1: download zip file from website
if(!file.exists("course_project"))dir.create("course_project")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile="./course_project/Clean_Data.zip")

##step 2: unzip data
listZip <- unzip("./course_project/Clean_Data.zip", exdir="./course_project")

##step 3: load data into R
train.x <- read.table("./course_project/UCI HAR Dataset/train/X_train.txt")
train.y <- read.table("./course_project/UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table("./course_project/UCI HAR Dataset/train/subject_train.txt")
test.x <- read.table("./course_project/UCI HAR Dataset/test/X_test.txt")
test.y <- read.table("./course_project/UCI HAR Dataset/test/y_test.txt")
test.subject <- read.table("./course_project/UCI HAR Dataset/test/subject_test.txt")

##step 4: merge data
trainData <- cbind(train.subject, train.y, train.x)
testData <- cbind(test.subject, test.y, test.x)
fullData <- rbind(trainData, testData)

#---------------------------------------------------
#2. Extract only the measurements on the mean and standard deviation for each measurement.

##step 1: load feature name into R
featureName <- read.table("./course_project/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[, 2]

##step 2: extract mean and standard deviation of each measurements
featureIndex <- grep(("mean()|std()"), featureName)
finalData <- fullData[, c(1, 2, featureIndex + 2)]
colnames(finalData) <- c("subject", "activity", featureName[featureIndex])

#---------------------------------------------------------
#3. Use descriptive activity names to name the activities in the data set

##step 1: load activity data into R
activityName <- read.table("./course_project/UCI HAR Dataset/activity_labels.txt")

##step 2: replace 1 to 6 with activity names
finalData$activity <- factor(finalData$activity, levels=activityName[,1], labels = activityName[,2])

#--------------------------------------------------------
#4. Label the data set with descriptive variable names.

names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "time", names(finalData))
names(finalData) <- gsub("^f", "frequence", names(finalData))
names(finalData) <- gsub("-mean", "Mean", names(finalData))
names(finalData) <- gsub("-std", "Std", names(finalData))

#--------------------------------------------------------------
#5. Create a second, dependent tidy data set with the average of each variable for each activity and each subject.
groupData <- finalData %>%
             group_by(subject, activity) %>%
             summarise_each(funs= mean) %>%
             arrange(subject, activity)
write.table(groupData, "./course_project/DataMean.txt", row.names = FALSE)
