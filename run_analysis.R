library("plyr")

mergeKeepOrder = function(x, y, ...) {
  dummy_name <- "dummy"
  x[, dummy_name] = 1:nrow(x)
  res = merge(x, y, all.x=TRUE, sort=FALSE, ...)
  res = res[order(res[, dummy_name]), ]
  res[, dummy_name] = NULL
  res
}

#***UNCOMMENT TO DOWNLOAD AS PART OF RUNNING THE SCRIPT.  
# download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="alldata.zip", method="curl");
# unzip("./alldata.zip")
# setwd("./UCI HAR Dataset")

#***OTHERWISE, JUST MAKE SURE YOU ARE RUNNING FROM THE Samsung data directory.
activity_labels  <- read.csv("./activity_labels.txt", sep=" ", header=FALSE)

#get features to use as column names
features_vect <- read.csv("features.txt",sep=" ", header=FALSE, as.is=TRUE)[,2]

#add subject and activity to it
features_vect<-append(features_vect, c("subject", "activity"), after=0)

#create vector for use with read.fwf
feature_widths <- rep(16,561)


#get training data
train_subjects  <- read.csv("train/subject_train.txt", header=FALSE)
train_labels  <- read.fwf("train/y_train.txt",widths=c(1),header=FALSE)
#replace ints with descriptive names
train_labels2 <- data.frame(mergeKeepOrder(train_labels,activity_labels)[,2])
train_X  <- read.fwf("train/X_train.txt", widths=feature_widths,header=FALSE,strip.white=TRUE)

#add subject and activity to training data set
train_all <- cbind(train_subjects,train_labels2,train_X)
colnames(train_all)<- features_vect

#get test data
test_subjects  <- read.csv("test/subject_test.txt", header=FALSE)
test_labels  <- read.fwf("test/y_test.txt",widths=c(1),header=FALSE)

#replace ints with descriptive names
test_labels2 <- data.frame(mergeKeepOrder(test_labels,activity_labels)[,2])
test_X  <- read.fwf("test/X_test.txt", widths=feature_widths, header=FALSE,strip.white=TRUE)

#add subject and activity to test data set
test_all <- cbind(test_subjects,test_labels2,test_X)
colnames(test_all)<- features_vect

#merge test and training sets
data_all<- rbind(train_all,test_all)

#Extract only the measurements on the mean and standard deviation for each measurement. 
cols_to_extract <- c(1,2, grep("std\\(\\)|mean\\(\\)",names(data_all)))
data_all <- data_all[,cols_to_extract]

#get mean for each measurement --aggregate data by subject and activity
agg_data_all <- ddply(data_all, c("subject","activity"), .fun=function(x) apply(x[3:dim(x)[2]],FUN=mean,MARGIN=2))

#order nicely
agg_data_all <- agg_data_all[order(agg_data_all[,1]),]

#remove row.names
#row.names(agg_data_all) <-NULL

write.table(agg_data_all,file="tidydata.txt",row.name=FALSE)



