# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following. 
# 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#1) Merges the training and the test sets to create one data set.

#Step 0) getwd(), cd to what you want, 
#step 1) unzip the files and save it under workig directory
#stop 2) read the data into a table and anything that has NULL or NOT Avail marked as na string.
df_x_train <- read.table('UCI HAR Dataset/train/X_train.txt', header = FALSE, na.strings=c('NULL','Not Available'))
df_y_train <- read.table('UCI HAR Dataset/train/y_train.txt', header = FALSE, na.strings=c('NULL','Not Available'))
df_sub_train <- read.table('UCI HAR Dataset/train/subject_train.txt', header = FALSE, na.strings=c('NULL','Not Available'))

df_x_test <- read.table('UCI HAR Dataset/test/X_test.txt', header = FALSE, na.strings=c('NULL','Not Available'))
df_y_test <- read.table('UCI HAR Dataset/test/y_test.txt', header = FALSE, na.strings=c('NULL','Not Available'))
df_sub_test <- read.table('UCI HAR Dataset/test/subject_test.txt', header = FALSE, na.strings=c('NULL','Not Available'))

#step 3) combine columns of each Train, Test.  first after lookig 
# at the data table they are columns. so I used cbind

train<-cbind(df_x_train, df_y_train,df_sub_train)
test<-cbind(df_x_test, df_y_test, df_sub_test)

#step 4) put train and test columns merge them into one data table
df_train_test = rbind(train, test)

#step 5) labeling columns by loading features.txt, feautures - factors
features <- read.table("./UCI HAR Dataset/features.txt")
firstColNames<-as.character(features$V2) #V2 columns has the names of the columns
colnames(df_train_test)[1:561]<-firstColNames #set column names of data
colnames(df_train_test)[562]<-"Activity"
colnames(df_train_test)[563]<-"Subject"
############################TEST CASE##################################
dim (df_train_test)
colnames(df_train_test)[563]
#######################END TEST ###################################

### 2) Extracts only the measurements on the mean and standard deviation
##  for each measurement. 
extract_features <- grepl("mean|std", features[,2])  #search in the string, | or, 
    #second column only names, logical array

meanSTD <- df_train_test[,extract_features] #creates meanSTD with only mean or std

##################TEST CASE########################3
##looking at the data table meanSTD and not seeing anything but mean|std in col names
###############end TEST################

### 3) Uses descriptive activity names to name the activities in the data set
labels_types_Activities <- read.table("UCI HAR Dataset/activity_labels.txt") 
## table labels_types_Activities created

j<-0 
for (i in labels_types_Activities[,2]) {
  j<-j+1
  df_train_test$Activity[df_train_test$Activity==j]<-i
  meanSTD$Activity[meanSTD$Activity==j]<-i
  
}



#### 4) Appropriately label the data set with descriptive variable names. 

names(df_train_test) <- gsub("^t", "Time", names(df_train_test))
names(df_train_test) <- gsub("^f", "Frequency", names(df_train_test))
names(df_train_test) <- gsub("sma", "Signal Magnitude Area", names(df_train_test))
names(df_train_test) <- gsub("Acc", "Accelerator", names(df_train_test))
names(df_train_test) <- gsub("Mag", "Magnitude", names(df_train_test))
names(meanSTD) <- gsub("^t", "Time", names(meanSTD))
names(meanSTD) <- gsub("^f", "Frequency", names(meanSTD))
names(meanSTD) <- gsub("sma", "Signal Magnitude Area", names(meanSTD))
names(meanSTD) <- gsub("Acc", "Accelerator", names(meanSTD))
names(meanSTD) <- gsub("Mag", "Magnitude", names(meanSTD))


activity_subject_mean<-df_train_test[, grep("Activity|Subject|mean\\(\\)", colnames(df_train_test))]

new_df<-melt(activity_subject_mean, id.vars = c("Activity", "Subject"))

activity_subject_mean<-dcast(new_df, Subject + Activity ~variable, mean)

write.table(activity_subject_mean, file = "tidy_data.txt",row.name=FALSE)

