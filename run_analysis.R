

setwd("C:/Users/MarkPetra/git/GetingCleaningData_CourseProject")
wd <- getwd()
datapath <- file.path(wd, "UCI HAR Dataset" )

## READING DATA FILES

#read variable names
filename <- "features.txt"
features <-read.table(file.path(datapath, filename),col.names = c("VarId", "VarName"))
featureNames <- as.character(features$VarName)

#read activity names
filename <- "activity_labels.txt"
actNames <-read.table(file.path(datapath, filename),col.names = c("ActivityId", "ActivityName"))

#we extract the featurenames that are related to the mean or std
#we do this by extracting names that contain "mean()" or "std()"
meanFNames <- featureNames[sapply(featureNames, regexpr, pattern = "mean\\(\\)") > 0]
stdFNames <- featureNames[sapply(featureNames, regexpr, pattern = "std\\(\\)") > 0]
selectedFNames <- c(meanFNames , stdFNames)
#remove the () in the names to make it cleaner
selectedFNames <- sub(pattern = "\\(\\)", replacement = "", x = selectedFNames)
length(selectedFNames)

#we create a vector to be used to only readin these col names from the train and test data files
#these files are fixed width files with 16 cols per variable
widthvector <- 16 * (2 *(featureNames %in% selectedFNames)-1)

#read train data
path <- file.path(datapath, "Train" )

filename <- "X_train.txt"
train_raw <- read.fwf(file.path(path, filename), width = widthvector, 
                      buffersize = 100, col.names = selectedFNames )
filename <- "y_train.txt"
train_y <- read.table(file.path(path, filename), col.names =c("ActivityId"))
filename <- "subject_train.txt"
train_subj <- read.table(file.path(path, filename), col.names = c("SubjectId"))

#read test data
path <- file.path(datapath, "Test" )

filename <- "X_test.txt"
test_raw <-  read.fwf(file.path(path, filename), width = widthvector, 
                     buffersize = 100, col.names = selectedFNames )
filename <- "y_test.txt"
test_y <- read.table(file.path(path, filename), col.names =c("ActivityId"))
filename <- "subject_test.txt"
test_subj <- read.table(file.path(path, filename),col.names = c("SubjectId"))

## Merging the train and test data
# We make on final dataset, test and train together, containing both the features and the activity and subject variable
# so total number of columns equals 66 + 1 + 1 = 68

totaldata <- rbind( cbind(train_raw, train_y, train_subj), cbind(test_raw, test_y, test_subj) )
#joining the activityNames to it
totaldata <- merge(totaldata, actNames, by.x = "ActivityId" , by.y = "ActivityId")

## From the totaldata set, we create a second, independent tidy data set with 
#the average of each variable for each activity and each subject.
meanDataPerActivitySubject <- aggregate(x = totaldata[,colnames(totaldata) %in% selectedFNames], 
                  by = list( ActivityName = totaldata$ActivityName, SubjectId = totaldata$SubjectId), 
                  FUN = mean)

meanDataPerActivitySubject <- meanDataPerActivitySubject[order(meanDataPerActivitySubject$ActivityName, 
                                                               meanDataPerActivitySubject$SubjectId),]
#Write this mean dataset to a file
filename <- "tidyMeanData.txt"
write.table(x = meanDataPerActivitySubject , filename, quote = F, row.names = F )

#also Write total dataset to a file
filename <- "tidyTotalData.txt"
write.table(x = totaldata , filename, quote = F, row.names = F )
