##
## Project 1 for Getting and Cleaning data
##

# Read the feature table
feature_table_file <- "features.txt"
features <- read.table(feature_table_file)

# locate the columns in the train and test data set that correspond to "mean" or "std" measurements
selectBoolArray <- grepl("*mean*", features[,2]) | grepl("*std*", features[,2])

# Read the X training data
x_train_file <- "train/X_train.txt"
x_train <- read.table(x_train_file)
# Read the activity data
y_train_file <- "train/y_train.txt"
y_train <- read.table(y_train_file)
names(y_train) <- "Activity"
# Read the subject data
subject_train_file <- "train/subject_train.txt"
subject_id_train <- read.table(subject_train_file)
names(subject_id_train) <- "Subject ID"
# Assign meaningful column names from the features data
colnames(x_train) <- as.character(features[,2])
# subset and keep only the mean and std variables
x_train_keep <- x_train[, selectBoolArray]
# add the activity and subject as an additional columns
x_train_keep <- cbind(x_train_keep, cbind(y_train, subject_id_train))

# Read the X test data
x_test_file <- "test/X_test.txt"
x_test <- read.table(x_test_file)
# Read the activity data
y_test_file <- "test/y_test.txt"
y_test <- read.table(y_test_file)
names(y_test) <- "Activity"
# Read the subject data
subject_test_file <- "test/subject_test.txt"
subject_id_test <- read.table(subject_test_file)
names(subject_id_test) <- "Subject ID"
# Assign meaningful column names from the features data
colnames(x_test) <- as.character(features[,2])
# subset and keep only the mean and std variables
x_test_keep <- x_test[, selectBoolArray]
# add the activity and subject as an additional columns
x_test_keep <- cbind(x_test_keep, cbind(y_test, subject_id_test))

# since the variable order is the same in x_train and x_test, we can merge the two uisng rbind
x_all <- rbind(x_train_keep, x_test_keep)
numbCol <- ncol(x_all)
activityCol <- numbCol - 1;

# Read in the acitivty labels
activity_labels <- read.table("activity_labels.txt")

# Replace the activity numerical code in the last column with the activity descriptive string
for (cnt in 1:nrow(activity_labels)) {
  numVal <- activity_labels[cnt,1]
  strVal <- as.character(activity_labels[cnt,2])
  indWithVal <- grep(numVal, x_all[,activityCol])
  x_all[indWithVal, activityCol] <- strVal
}

## Create the summary tidy data set
## Average each column of x_all by subject and keep merging
## There must be a better way of doing this but cannot figure it out right now
## first do the first column
namesX <- names(x_all)
tidy_data <- melt(tapply(x_all[,1], list(x_all$"Subject ID", x_all$Activity), mean))
names(tidy_data) <- c("Subject ID", "Activity", namesX[1])
for (cnt in 2:79) {
  tt <- melt(tapply(x_all[,cnt], list(x_all$"Subject ID", x_all$Activity), mean))
  names(tt) <- c("Subject ID", "Activity", namesX[cnt])
  tidy_data <- merge(tidy_data, tt, by = c("Subject ID", "Activity"))
}

write.table(tidy_data, "tidy_data.txt", row.name = FALSE, sep =",")
#final_data <- tidy_data[order("Subject ID", "Activity"),]
