#1.1: load packages and datasets; Do a check-up on the data

# load necessary packages 
library('dplyr') #data manipulation
library('ggplot2') #visualization

#set working directory
setwd("/Users/yanjunchen/Desktop/Kaggle/Titanic")

#import datasetsets 
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

#do a quick look-up on the dataset's framework
str(train)

#do a qucik statistical summary on key variables
summary(train$Sex)
table(train$Survived)

# So, it sees that more people died(549) than survived(342). Can/Shall we expect similir results
#in the testing dataset?

# check if the names(headers) of the training & testing datasets are the same
names(train)
names(test)


# Ok, they are the same. But the testing set doesn't have the "Survived" column (sure, that's what we're asked to predict!)
#So let's add a new column(Survived) to the testing set and set it as "NA"

test$Survived <- NA


#ok, let't combine the training and testing datasets so I can clean them tgt
combined <-bind_rows(train,test)

#do another look-up on the combined file
str(combined)
summary(combined)


# 2.1: 

#check missing values in Age:
table(is.na(train$Age))
#wow, there are 177 missing values. Let's replace them with the median values of Age

median.age <- median(train$Age, na.rm = TRUE) #(median = 28)
#replce all NAs with the median
train$Age[is.na(train$Age)] <- median.age


