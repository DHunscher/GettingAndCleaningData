# Getting and Cleaning Data Course Project README

## Source data
Obtained from the Human Activity Recogition Using Smartphones Data Set site: 
https://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Overview
The original data set is accompanied by README.txt and features_info.txt files.
These contain additional details regarding the original data set. 

From the original data set's README.txt, this description of the context:

"The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data."

# run_analysis.R 
 
 This script does the following:
 1. Merges the training and the test sets to create one data set.
 2. Extracts only the measurements on the mean and standard deviation for each 
       measurement. 
 3. Uses descriptive activity names to name the activities in the data set
 4. Appropriately labels the data set with descriptive variable names. 
 5. From the data set in step 4, creates a second, independent tidy data set 
       with the average of each variable for each activity and each subject.
 
## To run

From R or RStudio, Source the file run_analysis.R and then enter the command 
run_analysis(), or modify the defaults by entering any or all of the input 
arguments shown below.
 
## Inputs
       Data.URL               :        URL of the data set zip file.
       UCI.HAR.Dataset.Folder :        folder containing unzipped contents of 
                                       the UCI HAR data set; default  
                                       'UCI-HAR-Dataset'
       file.name              :        Output file name, default 
                                       'course.project.txt'

## Output                             
Tidy data set in the location specified by file.name argument.
