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

# it seems that more people died(549) than survived(342). Canwe expect similir results in the testing dataset?

# check if the names(headers) of the training & testing datasets are the same
names(train)
names(test)

#they are the same. But the testing set doesn't have the "Survived" column (sure, that's what we're asked to predict!)
#So let's add a new column(Survived) to the testing set and set it as "NA"
test$Survived <- NA

#combine the training and testing datasets so I can clean them tgt
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
#2.4.1 SibSp(# of siblings) & Survival

ggplot(train, aes(SibSp, fill = factor(Survived))) + geom_bar(stat = "count", position = 'dodge') +
  xlab("SibSp") + ylab("Count")

#2.4.2 Parch(# of parents) & Survival

ggplot(train, aes(Parch, fill = factor(Survived))) + geom_bar(stat = "count", position = 'dodge') +
  xlab("Parch") + ylab("Count")

# not a very clear trend. not sure about the significant either..

# combining siblings and parents data to see if the whole family size 
# has a clear trend associated with survival:

family <- combined$SibSp + combined$Parch
 #create a data frame of family and survival:

df_fami.sur <- data.frame(family = family[1:891], Survived = train$Survived)
tapply(df_fami.sur $Survived, df_fami.sur $family,mean)

# looks like for family size 0-3: survial rate increases as family size increases
# family size 4-10: not too clear

# 2.5 Name & survival
#the title of the name relects sex as well

title <- gsub('(.*, )|(\\..*)', '', combined$Name)

table(combined$Sex,title)

# it can be inferred from the table that "Miss" "Mr" "Mrs" are the most
# common titles. Assign some of the other titles as the most common ones
# and assign the rare ones as "others"

title[title == 'Ms'] <- 'Miss'
title[title == 'Mlle'] <- 'Miss'
title[title == 'Mme'] <- 'Mrs'

others <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

title[title %in% others]  <- 'others'


#visulize the relationship:
d <- data.frame(title = title[1:891], Survived = train$Survived)
ggplot(d, aes(title,fill = factor(Survived))) +
  geom_bar(stat = "count", position = 'dodge') +
  xlab("Title") + ylab("Count")

#it is obvious that ppl with "Mr" title has lower survival rate than ppl 
#with "Mrs" or "Miss" title. It can be inferred that sex does have some degree 
#of influence on survivial

# 2.6 Cabin & Survivial

#cabin data doesnt make too much sense. 
#to count how many cabins a passenger has

#assign a new var to combined$Cabin
cabin <- combined$Cabin
n = length(cabin)
for(i in 1:n) {
     if(nchar(cabin[i]) == 0) {
       cabin[i] = 0
       }
                   else{s = strsplit(cabin[i]," ") 
                   cabin[i] = length(s[[1]])
                   }
                }
table(cabin)

ggplot(combined, aes(cabin, fill = factor(Survived))) +  geom_bar(stat = "count", position = 'dodge') +
  xlab("cabin") + ylab("Count")

#there's grey area in the plot
#we can see that people with 0 cabin have a siginificantly lower survivial rate

#2.7 Fare & Survivial 
ggplot(train, aes(Fare,fill = factor(Survived))) +
  geom_histogram()

#2.8 Embarked & Suivival
ggplot(train, aes(Embarked,fill = factor(Survived))) + geom_bar(stat = "count", position = 'dodge') +
  xlab("Embarked") + ylab("Count")

#looks like people embarked 'c' has a higher survival rate

# 3. Model Building (June 21)

#independent variables(8): age, sex, Pclass, family, title, cabin, fare, embarked 
#dependent variable: survival 

# 3.1 feature engineering: 
fea.survived = train$Survived

fea.age = train$Age
test.age = test$Age

fea.sex = train$Sex
test.sex = test$Sex

fea.pclass = train$Pclass
test.pclass = test$Pclass

fea.family = family[1:891]
test.family = family[892:1309]

fea.title = title[1:891]
test.title = title[892:1309]

fea.cabin = cabin[1:891]
test.cabin = cabin[892:1309]

fea.fare = train$Fare
test.fare = test$Fare

fea.embarked = train$Embarked
test.embarked = test$Embarked

# 3.2 Building Random Forest

#Construct dataframe

model_train <- data.frame(survived = fea.survived, age = fea.age, sex = fea.sex, pclass = fea.pclass,
          family = fea.family, title = fea.title, cabin = fea.cabin, fare = fea.fare, embarked = fea.embarked)


# model 1: Random Forest
# install "randomForest" package:
.libPaths()
.libPaths( c(.libPaths(), "/Library/Frameworks/R.framework/Versions/3.2/Resources/library"))
install.packages("randomForest")

#building the model
set.seed(123)
model.rf <- randomForest(factor(survived) ~ age + sex + pclass +
                family + title + cabin + fare + embarked, data = model_train, na.action=na.roughfix )

#predicted results: 
rf.fitted = predict(model.rf)
results_rf = rep(NA, 891)
for(i in 1:891){results_rf[i] = as.integer(rf.fitted[[i]]) -1}

#see accuracy
mean(results_rf == train$Survived)
table(results_rf)

varImpPlot(model.rf, main = "RF_MODEL")
#title might the most influential variable


# 4. Prediction on testing data:
#constructing testing dataframe
test_data <- data.frame(age = test.age, sex = test.sex, pclass = test.pclass,
                        family = test.family, title = test.title, cabin = test.cabin, fare = test.fare, embarked = test.embarked)

# making prediction
rf.predict = predict(model.rf)
                     #, newdata =test_data)

#Prediction Results
results_rf.predict = rep(NA, 418)
for (i in 1:418) {results_rf.predict[i] = as.integer(rf.predict[[i]]) - 1}
table(results_rf.predict)

#results_rf.predict
#  0   1 
# 275 143 


#Conclusion(July 20)
