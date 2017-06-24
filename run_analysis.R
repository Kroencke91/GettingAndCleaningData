##############################################################
#Packages setup
##############################################################
require(dplyr)

require(lubridate)

require(tidyr)

require(stringr)

require(data.table)

##############################################################
#Initialize
##############################################################
rootdir <- "C:\\Users\\admin\\Documents\\R\\GettingAndCleaningData\\UCI_HAR_Dataset\\"

activitylabels <- tbl_df(read.delim2(paste0(rootdir,"activity_labels.txt"),header = FALSE, sep=" "))

names(activitylabels) <- c("actid","activity")

activitylabels <- select(activitylabels, activity, actid)

features <- tbl_df(read.delim2(paste0(rootdir,"features.txt"),header = FALSE, sep=" "))

names(features) <- c("index","feature")

features <- mutate(features,colname=make.unique(tolower(gsub("[(),-]","",features$feature))))

##############################################################
#TEST dataset setup
##############################################################
testdir <- paste0(rootdir,"test\\")

testsubject <- tbl_df(read.delim2(paste0(testdir,"subject_test.txt"),header = FALSE))

names(testsubject) <- c("subject")

testdat <- readLines(paste0(testdir,"X_test.txt")) %>%
  tbl_df() %>%
  separate(value,c(features$colname),sep=" ") %>%
  tbl_df()

testlabs <- tbl_df(read.delim2(paste0(testdir,"y_test.txt"),header = FALSE))  

names(testlabs) <- c("actid")

##############################################################
#TRAIN dataset setup
##############################################################
traindir <- paste0(rootdir,"train\\")

trainsubject <- tbl_df(read.delim2(paste0(traindir,"subject_train.txt"),header = FALSE))

names(trainsubject) <- c("subject")

traindat <- readLines(paste0(traindir,"X_train.txt")) %>%
  tbl_df() %>%
  separate(value,c(features$colname),sep=" ") %>%
  tbl_df()

trainlabs <- tbl_df(read.delim2(paste0(traindir,"y_train.txt"),header = FALSE))  

names(trainlabs) <- c("actid")

##############################################################
#Merge Test & Train Datasets - Requirement #1
##############################################################
alldat <- rbind(testdat, traindat)

alllabs <- rbind(testlabs, trainlabs)

alllabs <- left_join(alllabs,activitylabels, by = "actid")

alllabs <- select(alllabs, activity, actid)

allsubject <- rbind(testsubject, trainsubject)

##############################################################
#Extract Mean & Std Dev - Requirement #2
##############################################################
meanstddat <- cbind(select(alldat, contains("mean")),select(alldat, contains("std")))

##############################################################
#Descriptive Activity Names - Requirement #3
##############################################################
meanstddat <- tbl_df(cbind(allsubject,alllabs,meanstddat))

View(meanstddat)

##############################################################
#Descriptive Variable Names - Requirement #4
##############################################################

# Done above as part of TEST & TRAIN setup

##############################################################
#Tidy Average Dataset - Requirement #5
##############################################################
meanstdmeans <- meanstddat

meanstdmeans[,-2] <- apply(meanstdmeans[,-2],2,as.numeric) 

meanstdmeans$actid <- as.factor(meanstdmeans$actid)

meanstdmeans$subject <- as.factor(meanstdmeans$subject)

meanstdmeans <- aggregate(meanstdmeans[,-(1:3)], 
                          list(subject=meanstdmeans$subject,activity=meanstdmeans$activity,actid=meanstdmeans$actid), 
                          mean, na.rm = TRUE) %>% 
  tbl_df()
  
meanstdmeans <- meanstdmeans[order(meanstdmeans$subject, meanstdmeans$actid),]

names(meanstdmeans) <- c(names(meanstdmeans)[1:3],
                         apply(tbl_df(names(meanstdmeans))[-(1:3),], 2, paste0, "mean")[,1])

View(meanstdmeans)

##############################################################
#End of script
##############################################################
