#Kunal AGARWAL, 25/5/2016, Kaggle Competition

#Setting Working Directory
setwd('D:/Data Science/Kaggle Problems')

#Importing Data Sets
TitanicTrain <- read.csv("D:/Data Science/Kaggle Problems/Titanic DataSet/train.csv")
TitanicTest <- read.csv("D:/Data Science/Kaggle Problems/Titanic DataSet/test.csv")

# Investigating the Structure
str(TitanicTrain)
str(TitanicTest)

#Seeing Survived Variable
table(TitanicTrain$Survived)
prop.table(table(TitanicTrain$Survived))

#Assuming(predicting) no one dies in test set
TitanicTest$Survived <- rep(0,418)

#Creating Submission file
submit <- data.frame(PassengerId = TitanicTest$PassengerId, Survived = TitanicTest$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#Passengers survived based on gender, let's dive into sex variable
summary(TitanicTrain$Sex)
prop.table(table(TitanicTrain$Survived, TitanicTrain$Sex))

#Column-wise proportion
prop.table(table(TitanicTrain$Survived, TitanicTrain$Sex),2)

#Now we are creating model predicting that all females in test data survive
TitanicTest$Survived[TitanicTest$Sex == 'female'] <- 1
table(TitanicTest$Survived)

#Creating All males perished submission file
submit$Survived <- TitanicTest$Survived
table(submit$Survived)
write.csv(submit, file ="All males perished.csv", row.names = FALSE)

#Investigating Age Variable
summary(TitanicTrain$Age)

#Creating New Variable Child for Age < 18
TitanicTrain$Child <- 0
TitanicTrain$Child[TitanicTrain$Age < 18] <- 1
table(TitanicTrain$Child)

#Investigating Dependence of Survival on Age
table(TitanicTrain$Survived, TitanicTrain$Child)
prop.table(table(TitanicTrain$Survived, TitanicTrain$Child),2)

#Investing dependence of Survival on Age and Sex
aggregate(Survived ~ Child+Sex, data = TitanicTrain, FUN = sum) # no. of People survived based on diff. combi of age and sex
aggregate(Survived ~ Child+Sex, data = TitanicTrain, FUN = length) #no. of people in each group(survived+dead)

aggregate(Survived ~ Child+Sex, data = TitanicTrain, FUN = mean) #Proportion of people survived in each group

#COnverting Fare into Categorical Variable
TitanicTrain$Fare2 <- '30+'
TitanicTrain$Fare2[TitanicTrain$Fare < 30 & TitanicTrain$Fare >= 20] <- '20-30'
TitanicTrain$Fare2[TitanicTrain$Fare < 20 & TitanicTrain$Fare >= 10] <- '10-20'
TitanicTrain$Fare2[TitanicTrain$Fare < 10] <- '<10'


#Running Longer Aggregate with Sex, Fare2 and Pclass as Independent Variables
aggregate(Survived ~ Fare2 + Pclass + Sex, data=TitanicTrain, FUN=function(x) {sum(x)/length(x)})

#Gained Insight that female with Pclass ==3 and Fare2 >20 have less survival rate
#Therefore assigning 0 to female with Pclass ==3 and Fare2 >20 in test set
TitanicTest$Survived[TitanicTest$Sex == 'female' & TitanicTest$Pclass == 3 & TitanicTest$Fare >= 20] <- 0

#Writing to submission file 
submit$Survived <- TitanicTest$Survived
write.csv(submit, file = "FemalewithPClass3faregreaterthan20perished.csv",row.names = FALSE)

table(submit$Survived)
