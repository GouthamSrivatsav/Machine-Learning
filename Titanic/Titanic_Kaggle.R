

titanic.train = read.csv("train.csv",stringsAsFactors = FALSE,header=TRUE)
titanic.test = read.csv("test.csv",stringsAsFactors = FALSE,header=TRUE)
tail(titanic.train)
tail(titanic.test)
titanic.train$isTrainset = TRUE
tail(titanic.train)
titanic.test$isTrainset = FALSE
tail(titanic.test$isTrainset)
titanic.test$Survived = NA
titanic.full = rbind(titanic.train, titanic.test)
tail(titanic.full)
titanic.full[titanic.full$Embarked == '',"Embarked"] = "S"
Age.median = median(titanic.full$Age, na.rm = TRUE)

titanic.full[is.na(titanic.full$Age), "Age"] = Age.median

Fare.median = median(titanic.full$Fare, na.rm = TRUE)

titanic.full[is.na(titanic.full$Fare),"Fare"] = Fare.median


# Categorical Casting 

titanic.full$Pclass = as.factor(titanic.full$Pclass)
titanic.full$Embarked = as.factor(titanic.full$Embarked)
titanic.full$Sex = as.factor(titanic.full$Sex)

#Split dataset back into train and test

titanic.train = titanic.full[titanic.full$isTrainset == TRUE,]
titanic.test = titanic.full[titanic.full$isTrainset == FALSE,]


titanic.train$Survived = as.factor(titanic.train$Survived)

survived.equation = "Survived ~ Pclass + Age + Fare + Embarked + SibSp + Parch + Sex"
model.formula = as.formula(survived.equation)


install.packages("randomForest")

library(randomForest)

titanic.model = randomForest(formula = model.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 891 * 0.01)
features.model = "Pclass + Age + Fare + Embarked + SibSp + Parch + Sex"
Survived = predict(titanic.model, newdata = titanic.test)
PassengerId = titanic.test$PassengerId
output = as.data.frame(PassengerId)
output$Survived = Survived
write.csv(output,file = "Titanic_Kaggle_Submission.csv", row.names = FALSE)
