#1.1: load packages and datasets; Do a check-up on the data

# load necessary packages 
library('dplyr') #data manipulation
library('ggplot2') #visualization
library('ggthemes')#visualization
library('rpart') #regression trees

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


# 1.2: Data Prep

#check missing values in Age:
table(is.na(combined$Age))
#there are a lot of missing values. Let's replace them with the median values of Age
#(deleting might not be a good idea since the dataset is already small)

median.age <- median(combined$Age,na.rm = TRUE) #(median = 28)
#replce all NAs with the median
combined$Age[is.na(combined$Age)] <- median.age

#same rules applied to other 2 variables(Fare & Embarked) that have missing values: 
median.fare <- median(combined$Fare,na.rm = TRUE)
combined$Fare[is.na(combined$Fare)] <- median.fare


#way more people(mode) are embarking from S, let's just assign the 2 missing values to "S"


combined[combined$Embarked=='',"Embarked"] <- "S"

# 2 Exploratory data analysis (EAD)
# Use common senese and intuitions: due to "women and children first" code, 
# I'm gueesing Age and Sex might have an influence on the survival.

# Let's do some EAD 
# 2.1. Age vs. Survial: 

ggplot(combined[1:891,], aes(Age, fill = factor(Survived))) + geom_histogram() + xlab("Age") + ylab("Count")


#2.2. Sex vs Survival using the taaply function:

ggplot(combined[1:891,], aes(Sex, fill = factor(Survived))) + geom_bar(stat = "count", position = 'dodge') +
  xlab("Sex") + ylab("Count")

tapply(combined[1:891,]$Survived, combined[1:891,]$Sex, mean)
#female      male 
#0.7420382 0.1889081 

#seems that females has a larger survival rate

#2.3. Pclass vs Survival 

ggplot(combined[1:891,], aes(Pclass,fill = factor(Survived))) + geom_bar(stat = "count", position = 'dodge')
tapply(combined[1:891,]$Survived,train$Pclass,mean)
#seems that in terms of survivial rate, class 1>2>3 

#2.4. Family size vs Survival
family <- combined$SibSp + combined$Parch
#*BUG tapply(combined[1:891,]$Survived,combined[1:891,]$family,mean)



# Split the data back into training and testing
#train <- combined[1:891,]
#test <- combined[892:1309,]


#decision_tree <- ctree(
 # Survived ~ Pclass + Parch + Ticket + Cabin + Age + Embarked + Sex, 
 # data=train)


