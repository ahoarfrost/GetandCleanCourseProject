#You should create one R script called run_analysis.R that does the following:
    #Merges the training and the test sets to create one data set.
    #Extracts only the measurements on the mean and standard deviation for each measurement. 
    #Uses descriptive activity names to name the activities in the data set
    #Appropriately labels the data set with descriptive variable names. 
    #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


variable.colnames <- read.table(file="UCI HAR Dataset/features.txt",row.names=1,colClasses=c("character"))

##MERGE TRAINING AND TEST DATA SETS, and LABEL DATA SET WITH DESCRIPTIVE VARIABLE NAMES
#create columns for subject ID (from subject_??.txt), activity (from y_??.txt), and remaining parameters (from X_??.txt, should have 561 columns)
#do for both train and test sets and rbind the two

#subject ID
subject.id.train <- read.table(file="UCI HAR Dataset/train/subject_train.txt",col.names="SubjectID")
subject.id.test <- read.table(file="UCI HAR Dataset/test/subject_test.txt",col.names="SubjectID")

#activity
activity.train <- read.table(file="UCI HAR Dataset/train/y_train.txt",col.names="activity")
activity.test <- read.table(file="UCI HAR Dataset/test/y_test.txt",col.names="activity")

#remaining parameters, using descriptive colnames imported from variable.colnames
params.train <- read.table("UCI HAR Dataset/train/X_train.txt",col.names=variable.colnames[,1])
params.test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names=variable.colnames[,1])

#bind train tables togeter to make one master df; do same for test; rbind train and test datasets together
#train df
train.df <- cbind(subject.id.train, activity.train, params.train)
#test df
test.df <- cbind(subject.id.test, activity.test, params.test)
#master df
master.df <- rbind(train.df, test.df)


##EXTRACT ONLY MEASUREMENTS ON MEAN AND STD FOR EACH MEASUREMENT (and keep SubjectID and activity)
new.df <- master.df[,c(1,2,grep(pattern=c("*mean*|*std*"),x=colnames(master.df)))]


##REPLACE ACTIVITY NUMBER INDEXING WITH DESCRIPTIVE FACTORS using activity_labels.txt
activity.labels <- read.table("UCI HAR Dataset/activity_labels.txt",col.names=c("index","activity"))
new.df$activity <- gsub(pattern="1",replacement=activity.labels$activity[1],new.df$activity)
new.df$activity <- gsub(pattern="2",replacement=activity.labels$activity[2],new.df$activity)
new.df$activity <- gsub(pattern="3",replacement=activity.labels$activity[3],new.df$activity)
new.df$activity <- gsub(pattern="4",replacement=activity.labels$activity[4],new.df$activity)
new.df$activity <- gsub(pattern="5",replacement=activity.labels$activity[5],new.df$activity)
new.df$activity <- gsub(pattern="6",replacement=activity.labels$activity[6],new.df$activity)


#From the data set in new.df, creates a second, independent tidy data set with the average of each variable for each activity and each subject
new.df$activity <- factor(x=new.df$activity, levels=activity.labels$activity)
new.df$SubjectID <- factor(x=new.df$SubjectID, levels=1:30)
final.df <- expand.grid(levels(new.df$SubjectID),levels(new.df$activity))
colnames(final.df) <- c("SubjectID","activity")

vector <- numeric()
for (i in 1:nrow(final.df)) {
    a <- new.df[(new.df$SubjectID==final.df$SubjectID[i]) & (new.df$activity==final.df$activity[i]),]
    b <- colMeans(a[,3:ncol(a)])
    vector <- rbind(vector, b)
}

##FINAL TIDY DATA SET
tidy <- cbind(final.df,vector)
write.table(x=tidy,file="tidy.txt",row.names=FALSE)
