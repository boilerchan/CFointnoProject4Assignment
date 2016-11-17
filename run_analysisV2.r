install.packages("reshape2")
library(reshape2)

filename <- "getdata_dataset.zip"

##Download and unzip the dataset
if(!file.exists(filename))
{fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
 download.file(fileUrl, filename, method = "libcurl")
}
if (!file.exists("UCI HAR Dataset"))
{unzip(filename)}

##Load activity labels and features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

##Extract only the data on mean and standard deviation
featuresWanted <- grep("mean|std", features[,2]) # changed this
featuresWanted.names <- features[featuresWanted,2]
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)

##Load the datasets
train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

##Merge datasets and add labels
allData <- rbind(train, test)
colnames(allData) <- c("subject", "activity", featuresWanted.names)

allData$activity <- factor(allData$activity, levels = activityLabels[,1], 
                           labels = activityLabels[,2])

allData$activity <- as.character(allData$activity)

write.table(allData, "tidy.txt", row.names = FALSE, quote = FALSE)

averages <- data.frame()

for (activity in unique(allData$activity)) {
  for (subject in unique(allData$subject)) {
    subsetted_data <- allData[allData$activity == activity & allData$subject == subject, ]
    aggregates <- lapply(names(subsetted_data), function(colname) {
      if (colname == "subject") { return(subject) }
      if (colname == "activity") { return(activity) }
      mean(subsetted_data[[colname]])
    })
    names(aggregates) <- names(subsetted_data)
    aggregates <- as.data.frame(aggregates)
    averages <- plyr::rbind.fill(averages, aggregates)
  }
}

write.table(averages, "tidy_aggregated.txt", row.names = FALSE, quote = FALSE)
