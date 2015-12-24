if (!file.exists("getdata-projectfiles-UCIHARDataset.zip")) {
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, destfile = "./getdata-projectfiles-UCIHARDataset.zip") }

unzip("getdata-projectfiles-UCIHARDataset.zip")
library(data.table)
library(dplyr)


# Load file with list of column names
features <- tbl_df(fread(input = "./UCI HAR Dataset/features.txt")) %>% 
        mutate(tmp = make.names(V2, unique = TRUE, allow_ = TRUE)) %>%
        mutate(cnames = gsub("^t(\\w*)(\\.\\w*)(\\.{0,2})(\\.\\w*)?(.*)","\\1\\4\\.time\\2\\5",
                              gsub("^f(\\w*)(\\.\\w*)(\\.{0,2})(\\.\\w*)?(.*)","\\1\\4\\.freq\\2\\5", tmp))) %>%
        select(cnames)


# Load files with numeric observations
# Note: only columns that had functions mean and std applied in data generation are used
x_test <- fread(input = "./UCI HAR Dataset/test/X_test.txt")
setnames(x_test, unlist(select(features, cnames)))
x_test <- tbl_df(x_test) %>%
        select(ends_with("mean", ignore.case = TRUE), 
               ends_with("std", ignore.case = TRUE))

x_train <- fread(input = "./UCI HAR Dataset/train/X_train.txt")
setnames(x_train, unlist(select(features, cnames)))
x_train <- tbl_df(x_train) %>%
        select(ends_with("mean", ignore.case = TRUE), 
               ends_with("std", ignore.case = TRUE))


# Load files with subject designation for data
subject_test <- fread(input = "./UCI HAR Dataset/test/subject_test.txt")
setnames(subject_test,"subject")
subject_test <- tbl_df(subject_test)

subject_train <- fread(input = "./UCI HAR Dataset/train/subject_train.txt")
setnames(subject_train,"subject")
subject_train <- tbl_df(subject_train)


# Load file with activity descriptions
activity_labels <- fread(input = "./UCI HAR Dataset/activity_labels.txt")
setnames(activity_labels,c("activityid","activity"))
activity_labels <- tbl_df(activity_labels)

# Load files with activity designation for data
y_test <- fread(input = "./UCI HAR Dataset/test/y_test.txt")
setnames(y_test,"activityid")
y_test <- tbl_df(y_test) %>%
        left_join(activity_labels, by = "activityid")

y_train <- fread(input = "./UCI HAR Dataset/train/y_train.txt")
setnames(y_train,"activityid")
y_train <- tbl_df(y_train) %>%
        left_join(activity_labels, by = "activityid")


# Combine test dataset and train dataset
test_data <- subject_test %>%
        # mutate(dataset = "TEST") %>%
        bind_cols(select(y_test,-activityid)) %>%
        bind_cols(x_test)

train_data <- subject_train %>%
        # mutate(dataset = "TRAIN") %>%
        bind_cols(select(y_train,-activityid)) %>%
        bind_cols(x_train)



# Generate single, tidy dataset of test and training data 
# Mean summarized on subject and activity
activity_summary <- bind_rows(test_data, train_data) %>%
        group_by(subject, activity) %>%
        summarise_each(funs(mean))


# Write tidy set to text file
write.table(as.data.frame(activity_summary), file = "activity_summary.txt", quote = FALSE, row.names = FALSE)
