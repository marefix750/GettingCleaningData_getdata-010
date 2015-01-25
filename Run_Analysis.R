#preparing list of features that should be imported
col<- c(rep("NULL",561))
colnum<- c(1:6,
      41:46,
      81:86,
      121:126,
      161:166,
      201:202,
      214:215,
      227:228,
      240:241,
      253:254,
      266:271,
      345:350,
      424:429,
      503:504,
      516:517,
      529:530,
      542:543)
col[colnum] <- "numeric"

#import data from flat files
test <- read.table("./test/X_test.txt", colClasses = col)
test_sub <- read.table("./test/subject_test.txt")
test_act <- read.table("./test/y_test.txt")
train <- read.table("./train/X_train.txt", colClasses = col)
train_sub <- read.table ("./train/subject_train.txt")
train_act <- read.table("./train/y_train.txt")

#1. Merges training and test sets to create one data set
  test <- cbind(test_sub,test_act,test)
  train <- cbind(train_sub,train_act,train)
  data <- rbind(test,train)

#2. Uses descriptive activity names to name the activities in the data set
  data[,2] <- as.factor(data[,2])
  levels(data[,2])[1] <- "WALKING"
  levels(data[,2])[2] <- "WALKING_UPSTAIRS"
  levels(data[,2])[3] <- "WALKING_DOWNSTARIS"
  levels(data[,2])[4] <- "SITTING"
  levels(data[,2])[5] <- "STANDING"
  levels(data[,2])[6] <- "LAYING"

#3. Appropriately labels the data set with descriptive variable names.
  labels <- read.table("features.txt", colClasses = c("NULL", "character"))
  labels <- rbind("subject","activities",labels)
  colnum <- colnum +2
  colnum <- c(1,2,colnum)
  names(data) <- labels$V2[colnum]


#4. creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  #check reshape2 package availability
  if(!require(reshape2,quietly=TRUE)){
     install.packages("reshape2")
  }
  library(reshape2)
  #a. split data set based on subjects
  subdata <- split(data,data$subject)
  #b. melt and recast data to each subject to produce mean value of each variable
  for (i in 1:max(data$subject)){
    subdata[[i]] <- melt(subdata[[i]], id = c("subject","activities"))
    subdata[[i]] <- dcast(subdata[[i]],activities ~ variable, mean)
    subject <- rep(i,6)
    subdata[[i]]<-cbind(subject,subdata[[i]])
  }
  #c. Combine each subdata into new dataset
  avg_recognition <- data.frame()
  for (i in 1:length(subdata)){
    avg_recognition <- rbind(avg_recognition,subdata[[i]])
  }
  #Delete unused variable
  a <- ls()
  a <- a[a!="avg_recognition"]
  rm(list=a)
  rm(a)