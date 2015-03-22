# run_analysis.R 
# 
# This script does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each 
#       measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.
# 
# Inputs:
#       Data.URL               :        URL of the data set zip file.
#       UCI.HAR.Dataset.Folder :        folder containing unzipped contents of 
#                                       the UCI HAR data set.
#                               
library(data.table)
library(plyr)
library(stringr)
library(LaF)
library(reshape2)
library(dplyr)

# generic string substitute function for use with sapply
subst <- function(s,findme,replacewith) {
  s2 <- str_replace(s,findme,replacewith)
}

# get col numbers and names for means & standard deviation
# variables. Expand the names to be more descriptive of 
# the variables' content.
desiredCols <- function(UCI.HAR.Dataset.Folder) {
  fn <- paste(UCI.HAR.Dataset.Folder,"features.txt",sep="/")
  f <- read.delim(fn,sep=' ',header=F)
  x <- NULL
  for(i in 1:length(f[,1])) {
    if(!is.na(as.numeric(str_locate(f[i,2],"mean")[1,1]))) {
      x <- rbind(x,as.numeric(f[i,1]))
    }
  }
  for(i in 1:length(f[,1])) {
    if(!is.na(as.numeric(str_locate(f[i,2],"std")[1,1]))) {
      x <- rbind(x,as.numeric(f[i,1]))
    }
  }
  n <- as.character(f[sort(x),2])
  n <- as.character(sapply(n,subst,'BodyBody','Body'))
  n <- as.character(sapply(n,subst,'Body','Body '))
  n <- as.character(sapply(n,subst,'Acc','Acceleration '))
  n <- as.character(sapply(n,subst,'Gyro','Gyroscopic '))
  n <- as.character(sapply(n,subst,'std','Standard Deviation '))
  n <- as.character(sapply(n,subst,'Mag','Magnitude '))
  n <- as.character(sapply(n,subst,'Gravity','Gravity '))
  n <- as.character(sapply(n,subst,'Jerk','Jerk '))
  n <- as.character(sapply(n,subst,'X$','X Axis'))
  n <- as.character(sapply(n,subst,'Y$','Y Axis'))
  n <- as.character(sapply(n,subst,'Z$','Z Axis'))
  n <- as.character(sapply(n,subst,'-',' '))
  n <- as.character(sapply(n,subst,'^t','Time domain:       '))
  n <- as.character(sapply(n,subst,'meanFreq','Mean of Frequency '))
  n <- as.character(sapply(n,subst,'mean','Mean'))
  n <- as.character(sapply(n,subst,'^f','Frequency domain: '))
  n <- as.character(sapply(n,subst,'[()]',''))
  n <- as.character(sapply(n,subst,'[()]',''))
  n <- as.character(sapply(n,subst,' $',''))
  n <- as.character(sapply(n,subst,' -','-'))
  n <- as.character(sapply(n,subst,'  ',' '))
  return(list(sort(x),n))
}

getDatasetFilename <- function(UCI.HAR.Dataset.Folder,set.Folder) {
  fn <- sprintf("%s/%s/X_%s.txt",UCI.HAR.Dataset.Folder,
                set.Folder,
                set.Folder)
  return(fn)
}

getActivityColumnFilename <- function(UCI.HAR.Dataset.Folder,set.Folder) {
  fn <- sprintf("%s/%s/y_%s.txt",UCI.HAR.Dataset.Folder,
                set.Folder,
                set.Folder)
  return(fn)
}

getActivityNamesFilename <- function(UCI.HAR.Dataset.Folder) {
  fn <- sprintf("%s/activity_labels.txt",UCI.HAR.Dataset.Folder)
  return(fn)
}

getSubjectColumnFilename <- function(UCI.HAR.Dataset.Folder,set.Folder) {
  fn <- sprintf("%s/%s/subject_%s.txt",UCI.HAR.Dataset.Folder,
                set.Folder,
                set.Folder)
  return(fn)
}

# get the activity column for either the test or training set.
getActivityColumn <- function(UCI.HAR.Dataset.Folder,set.Folder) {
  fn <- getActivityNamesFilename(UCI.HAR.Dataset.Folder)
  activityNames <- read.delim(fn,sep=" ",header=FALSE)
  activityNames[,1] <- as.integer(activityNames[,1])
  activityNames[,2] <- as.character(activityNames[,2])
  fn <- getActivityColumnFilename(UCI.HAR.Dataset.Folder,set.Folder)
  activityCol <- read.delim(fn,header=FALSE)
  names <- NULL
  for (val in activityCol[,1]) {
    for (i in 1:length(activityNames[,1])) {
      if (val == activityNames[i,1]) {
        names <- rbind(names,activityNames[i,2])
        break
      }
    }
  }
  return(names)
}

# get the subject column for either the test or training set.
getSubjectColumn <- function(UCI.HAR.Dataset.Folder,set.Folder) {
  fn <- getSubjectColumnFilename(UCI.HAR.Dataset.Folder,set.Folder)
  subjectCol <- read.delim(fn,header=FALSE)
  subjectCol[,1] <- as.integer(subjectCol[,1])
  return(subjectCol)
}

# Utility function to get either the test or training set.
# reduce to the desired set of columns and add columns for 
# the subject and activity.
getDataSet <- function(UCI.HAR.Dataset.Folder,set.Folder) {
  fn <- getDatasetFilename(UCI.HAR.Dataset.Folder,
                           set.Folder)
  subjectCol <- getSubjectColumn(UCI.HAR.Dataset.Folder,
                                 set.Folder)
  activityCol <- getActivityColumn(UCI.HAR.Dataset.Folder,
                                 set.Folder)
  numRows <- length(activityCol)
  x <- desiredCols(UCI.HAR.Dataset.Folder)
  
  colNumbers <- x[[1]]
  colNames <- c("Subject","Activity",x[[2]])
  
  bufSize <- 1000
  dataSet <- NULL
  startRow <- 1
  laf <- laf_open_fwf(fn,
                      column_types=rep("double",561),
                      column_widths=rep(16,561))
  
  # loop through the very large data set, bufSize rows at a time,
  # reducing to just the columns we need.
  while(startRow < numRows) {
    rows <- next_block(laf,nrows=bufSize)
    rows <- rows[,colNumbers]
    dataSet <- rbind(dataSet,rows)
    startRow <- startRow + bufSize
  }
  # Also add columns to identify the subject and activity for each row
  dataSet <- cbind(activityCol,dataSet)
  dataSet <- cbind(subjectCol,dataSet)
  colnames(dataSet) <- colNames
  return(dataSet)
}

# validate inputs to top-level function. Make
# sure directory structure exists and the necessary files
# are located therein.
validateInputs <- function(
  UCI.HAR.Dataset.Folder
) {
  if (!file_test('-d',UCI.HAR.Dataset.Folder)) {
    return(FALSE)
  }
  if (!file_test('-d',paste(UCI.HAR.Dataset.Folder,'test',sep='/'))) {
    return(FALSE)
  }
  if (!file_test('-d',paste(UCI.HAR.Dataset.Folder,'train',sep='/'))) {
    return(FALSE)
  }
  if (!file_test('-f',getDatasetFilename(UCI.HAR.Dataset.Folder,'train'))) {
    return(FALSE)
  }
  if (!file_test('-f',getDatasetFilename(UCI.HAR.Dataset.Folder,'test'))) {
    return(FALSE)
  }
  if (!file_test('-f',getActivityColumnFilename(UCI.HAR.Dataset.Folder,'train'))) {
    return(FALSE)
  }
  if (!file_test('-f',getActivityColumnFilename(UCI.HAR.Dataset.Folder,'test'))) {
    return(FALSE)
  }
  if (!file_test('-f',getSubjectColumnFilename(UCI.HAR.Dataset.Folder,'train'))) {
    return(FALSE)
  }
  if (!file_test('-f',getSubjectColumnFilename(UCI.HAR.Dataset.Folder,'test'))) {
    return(FALSE)
  }
  if (!file_test('-f',paste(UCI.HAR.Dataset.Folder,'features.txt',sep='/'))) {
    return(FALSE)
  }
  return(TRUE)
}

# merge the training and test sets.
mergeTrainingAndTestSets <- function(
  UCI.HAR.Dataset.Folder) {
  # Merge the training and the test sets to create one data set.
  dtTest  <- getDataSet(UCI.HAR.Dataset.Folder,'test')
  dtTrain <- getDataSet(UCI.HAR.Dataset.Folder,'train')
  dts <- rbind(dtTrain,dtTest)
  return(dts)
}

# Using the merged data set, create the much smaller tidy
# data set.
createTidyDataSet <- function(mergedDataSet) {
  # Create a second, independent tidy data set with the 
  # average of each variable for each activity and each subject.
  td2 <- melt(mergedDataSet,
              id=colnames(mergedDataSet)[1:2],
              measure.vars=colnames(mergedDataSet)
                  [3:length(colnames(mergedDataSet))])
  td3 <- arrange(td2,Subject,Activity,variable)
  td4 <- group_by(td3,Subject,Activity,variable)
  td5 <- summarize(td4,meanValue = mean(value))
  v1 <- td5[,1]
  v2 <- td5[,2]
  v3 <- td5[,3]
  v4 <- td5[,4]
  tidyDataSet <- data.table(v1,v2,v3,v4)
  return(tidyDataSet)
}

# download UCI HAR data set zip file; unzip to a subdirectory
# of your choice.
downloadAndUnzipDataFile <- function(Data.URL=
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                                     UCI.HAR.Dataset.Folder='UCI-HAR-Dataset') {
  zipfileFolder <- "UCI HAR Dataset"
  if (file.exists(zipfileFolder)) {
    unlink(zipfileFolder,
           recursive=TRUE,
           force=TRUE)
  }
  
  if (file.exists(UCI.HAR.Dataset.Folder)) {
    unlink(UCI.HAR.Dataset.Folder,
           recursive=TRUE,
           force=TRUE)
  }
  
  zipfile <- paste(UCI.HAR.Dataset.Folder,
                   "zip",
                   sep=".")
  download.file(url=Data.URL,
                destfile=zipfile,
                method="curl",
                mode="wb")
  unzip(zipfile)
  file.rename(zipfileFolder,UCI.HAR.Dataset.Folder)
}

##################
###### MAIN ######
##################

# run the analysis end to end. The output is a tidy data set in the
# file specified in the file.name argument. 
run_analysis <- function(Data.URL=
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                         UCI.HAR.Dataset.Folder='UCI-HAR-Dataset',
                         file.name='course.project.txt'
                ) {
        mergedDataSet <- NULL
        tidyDataSet <- NULL
      
        downloadAndUnzipDataFile(Data.URL,UCI.HAR.Dataset.Folder)
        
        ok <- validateInputs(UCI.HAR.Dataset.Folder)
        
        if ( !ok ) {
          stop("Invalid input")
        }
        
        mergedDataSet <- mergeTrainingAndTestSets(UCI.HAR.Dataset.Folder)
        
        tidyDataSet <- createTidyDataSet(mergedDataSet)
        
        write.table(tidyDataSet,file.name,sep=",",row.names=FALSE,eol="\r\n")
        
        return(TRUE)
}



