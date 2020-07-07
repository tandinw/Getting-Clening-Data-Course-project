##creating folder data in the directory if it doesnot exists.##

if(!file.exists("./data")){dir.create("./data")}
file.exists("./data")

##getting data from URL##

fileurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile = "./data/Dataset.zip")

##unzipping the Dataset #####

unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

#************************************************************************************************
## step: 1 ***** Merge Training and Test sets to a single SET*****

##Reading the datasets: Training 

x_train<-read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("./data/UCI HAR Dataset/train/Y_train.txt")
subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")  

##Reading the datasets: Test

x_test<-read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./data/UCI HAR Dataset/test/Y_test.txt")
subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")  

## Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

## Reading activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

##Assigning colnames to data

colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

##Merging all data in one set

mrg_train <- cbind(y_train, subject_train, x_train)
mrg_test <- cbind(y_test, subject_test, x_test)
Data <- rbind(mrg_train, mrg_test)


## step: 2 ***** Extracting only the measurements on the mean and standard deviation for each measurement*****

colNames<-colnames(Data)

mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

## subsetting required data

mean_and_stdData<- Data[, mean_and_std==T]



##Step:3 *****Uses descriptive activity names to name the activities in the data set*****

setWithActivityNames <- merge(mean_and_stdData, activityLabels,
                              by='activityId',
                              all.x=TRUE)

##Step: 4 ****Appropriately labels the data set with descriptive variable names.****

##named in previous steps

##Step:5 **** From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.****

##5 Making a second tidy data set

secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]

##5 Writing second tidy data set in txt file

write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
