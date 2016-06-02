#Kunal AGARWAL, 25/5/2016, Kaggle Titanic Competition

#Recursive Partioning and Regression Trees uses CART desicion tree algo
library(rpart)

#Creating CART Model on Training data

CARTmodel <-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                    Fare + Embarked, data=TitanicTrain, method="class")

plot(CARTmodel)
text(CARTmodel)

#Installing Packages for better Graphics
install.packages('rattle')

install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Plotting Desicion Tree
fancyRpartPlot(CARTmodel)

#Making Predictions

PredictCART = predict(CARTmodel, newdata = TitanicTest, type = 'class')
table(PredictCART)

#Writing Predictions to External file
submit <- data.frame(PassengerId = TitanicTest$PassengerId, Survived = PredictCART)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#Overfitting went too far with tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=TitanicTrain,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
?rpart.control()

fit1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=TitanicTrain,
             method="class", control=rpart.control(minsplit= 10, cp = 0.5))

new.fit1 <- prp(fit1,snip=TRUE)$obj
fancyRpartPlot(new.fit1)
