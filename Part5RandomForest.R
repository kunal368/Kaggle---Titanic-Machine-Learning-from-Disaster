# Kunal Agarwal 26/5/2016 (Using same FeatureEngineering variables from Part 4)
#Titanic : Part5 : Random Forests

setwd("D:/Data Science/Kaggle Problems/Titanic DataSet")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Install and load required packages for decision trees and forests
library(rpart)
install.packages('randomForest')
library(randomForest)
install.packages('party')
library(party)

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
# Combine small title groups
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)

#Inspecting Age Variable
summary(combi$Age)

#Filling NA's for Age Variable By using Desicion Tree
AgeFit <- rpart(Age ~ Pclass+Sex+SibSp+Parch+Fare+Embarked+
                Title+FamilySize, data = combi[!is.na(combi$Age), ], method = "anova")

combi$Age[is.na(combi$Age)] <- predict(AgeFit, newdata = combi[is.na(combi$Age),])

summary(combi)

#Fill in Embarked blanks
summary(combi$Embarked)
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

# Fill in Fare NAs
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Reducing Levels of Family ID as <32 levels allowed in Random Forest
combi$FamilyID2 <- combi$FamilyID
#converting to String 
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
#converting back to factor
combi$FamilyID2 <- factor(combi$FamilyID2)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# Build Random Forest Ensemble
set.seed(521)
fit <- randomForest(as.factor(Survived) ~ Pclass+Sex+Age+SibSp+Parch+
      Fare+Embarked+Title+FamilySize+FamilyID2, data =train,importance =TRUE,ntree =2000)

#Look at Variable Importance 
varImpPlot(fit)

#Predicting for test set and writing submission file
Predictfit <- predict(fit, newdata = test)
table(Predictfit)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Predictfit)
write.csv(submit, file = "firstrandomforest.csv", row.names = FALSE)

# Build condition inference tree Random Forest
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
          Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 

#Prediction making and writing to file
Prediction = predict(fit, test,OOB = TRUE, type = 'response')
table(Prediction)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)
