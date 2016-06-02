#Kunal Agarwal 25/5/2016 Titanic Kaggle Problem 
#Part4: Feature Engineering

#setting working directory
setwd("D:/Data Science/Kaggle Problems/Titanic DataSet")

#Loading Data Sets
train <- read.csv("D:/Data Science/Kaggle Problems/Titanic DataSet/train.csv")
test <- read.csv("D:/Data Science/Kaggle Problems/Titanic DataSet/test.csv")

#Combining training and testing DataSets
test$Survived <- NA
combi <- rbind(train, test)

#Getting 1st name
combi$Name[1]

#Converting Name Variable from Factors to text string
combi$Name <- as.character(combi$Name)
combi$Name[1]

#Breaking the String into Substrings Separated by ., and getting the title i.e 2 element 
temp <-strsplit(combi$Name[1], split ='[,.]') #doubly stacked Matrix got created
temp1 <- strsplit(combi$Name, split = '[,.]') #
temp1[[529]][2]#Getting 2 element from doubly stacked Matrix


strsplit(combi$Name[1], split ='[,.]')[[1]]
strsplit(combi$Name[1], split ='[,.]')[[1]][2]

#Getting Title from every row of name Variable (Engineered Variable:Title)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
str(combi$Title)

#Removing Spaces from Titles at beginning
combi$Title <- sub(' ', '', combi$Title)
str(combi$Title)

#Inspecting New Feature
table(combi$Title)

#Combining small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir','Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess' )] <- 'Lady'

#Converting into Factors
combi$Title <- as.factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#Engineered Variable: Family

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

#Inspect Family ID
table(combi$FamilyID)

#Delete Erronous familyIDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

#Covert to factor
combi$FamilyID <- factor(combi$FamilyID)

#Splitting Train and test set
train <- combi[1:891, ]
test <- combi[892:1309, ]

# Install and load required packages for fancy decision tree plotting
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Desicion tree making prediction using new variables
Titanicmodel = rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+
                  Title+FamilySize+FamilyID, data =train, method = "class")

fancyRpartPlot(Titanicmodel)

#Making Predictions
PredictTitanicmodel <- predict(Titanicmodel, newdata = test, type = "class")
test$Survived <- PredictTitanicmodel

#Making Submission file
submit <- data.frame(PassengerId = test$PassengerId, Survived = PredictTitanicmodel) 
write.csv(submit, file ="engineeredfeaturetree.csv", row.names = FALSE)
########################################3

