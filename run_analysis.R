#I assume all the files locaded in my working directory
#read all the files into R
X_test <- read.table("./HAR/X_test.txt")
Y_test <- read.table("./HAR/Y_test.txt")
subject_test <- read.table("./HAR/subject_test.txt")
X_train <- read.table("./HAR/X_train.txt")
Y_train <- read.table("./HAR/Y_train.txt")
subject_train <- read.table("./HAR/subject_train.txt")
feature <- read.table("./HAR/features.txt",header=F)
#name the traning and test data from X_ files
colnames(X_test) <- feature[,2]
colnames(X_train) <- feature[,2]
#matching the activity code and activity and combine them,and merge all the files into one
#run these on both traning and test data
activityTest <- vector(length=nrow(Y_test))
for(i in 1:nrow(Y_test)){
        if(Y_test[i,1]==1){activityTest[i]="WALKING"}
        if(Y_test[i,1]==2){activityTest[i]="WALKING_UP"}
        if(Y_test[i,1]==3){activityTest[i]="WALKING_DOWN"}
        if(Y_test[i,1]==4){activityTest[i]="SITTING"}
        if(Y_test[i,1]==5){activityTest[i]="STANDING"}
        if(Y_test[i,1]==6){activityTest[i]="LAYING"}        
}
activityNew <- cbind(Y_test,activityTest)  #matched activity ID and activity
colnames(activityNew)[1:2] <- c("activityCode","activity")
bind <- cbind(subject_test,activityNew)
colnames(bind)[1] <- "subject"
mergedTest <- cbind(bind,X_test)  #merged test data
activityTrain <- vector(length=nrow(Y_train))
for(i in 1:nrow(Y_train)){
        if(Y_train[i,1]==1){activityTrain[i]="WALKING"}
        if(Y_train[i,1]==2){activityTrain[i]="WALKING_UP"}
        if(Y_train[i,1]==3){activityTrain[i]="WALKING_DOWN"}
        if(Y_train[i,1]==4){activityTrain[i]="SITTING"}
        if(Y_train[i,1]==5){activityTrain[i]="STANDING"}
        if(Y_train[i,1]==6){activityTrain[i]="LAYING"}        
}
activityNew2 <- cbind(Y_train,activityTrain)
colnames(activityNew2)[1:2] <- c("activityCode","activity")
bind2 <- cbind(subject_train,activityNew2)
colnames(bind2)[1] <- "subject"
mergedTrain <- cbind(bind2,X_train)
mergedWhole <- rbind(mergedTrain,mergedTest)
#select the data end with "mean();std()" using "find" function of text and record it
meanStd <- c(1:9,44:49,84:89,124:129,164:169,204:205,217:218,230:231,
             243:244,256:257,269:274,348:353,427:432,506:507,519:520,
             532:533,545,546)
mergedTidy <- mergedWhole[,meanStd]
#reshape the tidy data and further summarize it using mean funtion
library(reshape2)
melten <- melt(mergedTidy,id=c("subject","activityCode","activity"))
meanTidy <- dcast(melten,subject+activityCode+activity~variable,mean) #read more about reshape2 pac
#sink all the output into a local file, max print require adjusting
options(max.print=5.5E5)
sink("./HAR/runAnalysisOutput.txt")
meanTidy
sink()