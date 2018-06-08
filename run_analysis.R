library(data.table)
library(reshape2)
library(stringi)

# The approach is to first identify the needed mean and SD variables 
#    so that only those variables can be loaded from the data files.

activityLabels <- fread("./activity_labels.txt", col.names = c("classLabels", "activityName"))
features <- fread("./features.txt", col.names = c("index", "featureNames"))

# 2.	Extracts only the measurements on the mean and standard deviation for each measurement.
featuresNeeded <- grep("(mean|std)\\(\\)", features[, featureNames])  # select mean and SD
measurements <- features[featuresNeeded, featureNames]
measurements <- gsub('[()]', '', measurements)  # remove brackets

# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names.
# read only needed columns
x_train <- fread("./train/X_train.txt")[, featuresNeeded, with = FALSE]
# set up new names 
setnames(x_train, colnames(x_train), measurements)
y_train <- fread("./train/y_train.txt", col.names=c("Activity"))
s_train <- fread("./train/subject_train.txt", col.names = c("SubjectNum"))

#read only needed columns
x_test <- fread("./test/X_test.txt")[, featuresNeeded, with = FALSE]
# set up new names 
setnames(x_test, colnames(x_test), measurements)
y_test <- fread("./test/y_test.txt", col.names=c("Activity"))
s_test <- fread("./test/subject_test.txt", col.names = c("SubjectNum"))

# Subjects, Actviity, Measurements (column wise addition)
train <- cbind(s_train, y_train, x_train)
test <- cbind(s_test, y_test, x_test)

# 1.	Merges the training and the test sets to create one data set.
# combine training and testing data (rowwise addition)
data <- rbind(train, test)

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

data[["Activity"]] <- factor(data[, Activity], levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])
data[["SubjectNum"]] <- as.factor(data[, SubjectNum])

data1  <- melt(data = data, id = c("SubjectNum", "Activity"))
data2 <-  dcast(data = data1, SubjectNum + Activity ~ variable, fun.aggregate = mean)

fwrite(x = data2, file = "tidy.txt", quote = FALSE, row.name = FALSE)
