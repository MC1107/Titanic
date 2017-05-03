# load necessary packages (decision trees?)

#set working directory
setwd("/Users/yanjunchen/Desktop/Kaggle/Titanic")

#import datasetsets 
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#do a quick look up on the dataset's framework
str(train)

#do a qucik statistical summary on key variables
summary(train$Sex)
table(train$Survived)

#ok, let't combine the training and testing datasets
combined <-bind_rows(train,test)

#do another look-up on the combined file
str(combined)
summary(combined)
