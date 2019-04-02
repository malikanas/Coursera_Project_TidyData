library(dplyr)

# reading train data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Sb_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# reading test data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Sb_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# reading data description
variable_names <- read.table("./UCI HAR Dataset/features.txt")

# reading activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Step_1. Merge the training and test to create one data set.
X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
Sb_total <- rbind(Sb_train, Sb_test)

# Step_2. Extracts only the measurements on the mean and standard deviation for each measurements.
selected_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
X_total <- X_total[,selected_var[,1]]

# Step_3. Useing descriptive activity names to name the activities in the data set
colnames(Y_total) <- "activity"
Y_total$activitylabel <- factor(Y_total$activity, labels = as.character(activity_labels[,2]))
activitylabel <- Y_total[,-1]

# Step_4. label the data set with descriptive variable names.
colnames(X_total) <- variable_names[selected_var[,1],2]

# Step_5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(Sb_total) <- "subject"
total <- cbind(X_total, activitylabel, Sb_total)
total_mean <- total %>% group_by(activitylabel, subject) %>% summarize_each(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)