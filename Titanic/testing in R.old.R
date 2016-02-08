#backing up inputs
setwd("C:/Users/Laurae/Documents/Data Science/Kaggle/Titanic/")
train <- read.table("train.csv", sep = ",", header = TRUE)
test <- read.table("test.csv", sep = ",", header = TRUE)
train2 <- train
test2 <- test
gendermodel2 <- gendermodel
#rewrite data
train <- train2
test <- test2
gendermodel <- gendermodel2
#cleaning up original data
train[, "Survived"] <- as.factor(train[, "Survived"])
train[, "Pclass"] <- as.factor(train[, "Pclass"])
train[, "SibSp"] <- as.factor(train[, "SibSp"])
train[, "Parch"] <- as.factor(train[, "Parch"])
str(train)
train = train[, !names(train) %in% c("PassengerId","Ticket","Name")]
levels(train[, "Survived"]) <- c("No", "Yes")
train[, "Cabin"] <- strtrim(train[, "Cabin"], 1)
train[, "Cabin"] <- as.factor(train[, "Cabin"])
library(caret)
inTrainingSet <- createDataPartition(train$Survived, p = .75, list = FALSE)
churnTrain <- train[ inTrainingSet,]
churnTest <- train[-inTrainingSet,]
numerics <- c("Age", "Fare")
churnTrain$Cabin[which(!(churnTest$Cabin %in% levels(churnTrain[,"Cabin"])))] <- NA
procValues <- preProcess(churnTrain[,numerics], method = c("center", "scale"))
trainScaled <- predict(procValues, churnTrain[,numerics])
testScaled <- predict(procValues, churnTest[,numerics])
#parallelize task
library(doParallel)
registerDoParallel()
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
gbmTune <- train(Survived ~ ., data = churnTrain,
                 method = "gbm",
                 metric = "ROC",
                 verbose = FALSE,
                 trControl = ctrl)
c5Model <- train(Survived ~ ., data = churnTrain,
                 method = "C5.0",
                 metric = "ROC",
                 tuneLength = 10,
                 trControl = ctrl)
#summary data
getTrainPerf(gbmTune)
getTrainPerf(c5Model)
varImp(gbmTune, scale = FALSE)
varImp(c5Model, scale = FALSE)
gbmPred <- predict.train(gbmTune, churnTest, na.action = na.pass)
str(gbmPred)
summary(gbmPred)
confusionMatrix(gbmPred, churnTest$Survived)
C50Pred <- predict.train(gbmTune, churnTest, na.action = na.pass)
str(C50Pred)
summary(C50Pred)
confusionMatrix(C50Pred, churnTest$Survived)
#testing data
test[, "Pclass"] <- as.factor(test[, "Pclass"])
test[, "SibSp"] <- as.factor(test[, "SibSp"])
test[, "Parch"] <- as.factor(test[, "Parch"])
test[, "Cabin"] <- strtrim(test[, "Cabin"], 1)
test[, "Cabin"] <- as.factor(test[, "Cabin"])
str(test)
test = test[, !names(test) %in% c("PassengerId","Ticket","Name")]
test$Parch[test$Parch == 9] <- 6
test$Cabin[which(!(test$Cabin %in% levels(churnTrain[,"Cabin"])))] <- NA
test$Parch[which(!(test$Parch %in% levels(churnTrain[,"Parch"])))] <- NA
test$Embarked[which(!(test$Embarked %in% levels(churnTrain[,"Embarked"])))] <- NA
#unique(train$Parch)
#unique(test$Parch)
gbmPred2 <- predict.train(gbmTune, test, na.action = na.pass)
str(gbmPred2)
summary(gbmPred2)
C50Pred2 <- predict.train(c5Model, test, na.action = na.pass)
str(C50Pred2)
summary(C50Pred2)
#gbm
gendermodel$Survived[gbmPred2 == "No"] <- 0
gendermodel$Survived[gbmPred2 == "Yes"] <- 1
write.csv(gendermodel, file = "C:/Users/Laurae/Documents/Data Science/Kaggle/Titanic/Testing.csv", row.names = FALSE)
#C5.0
gendermodel$Survived[C50Pred2 == "No"] <- 0
gendermodel$Survived[C50Pred2 == "Yes"] <- 1
write.csv(gendermodel, file = "C:/Users/Laurae/Documents/Data Science/Kaggle/Titanic/Testing.csv", row.names = FALSE)
str(gendermodel)


#final GBM

train <- train2
test <- test2
gendermodel <- gendermodel2
#cleaning up original data
train[, "Survived"] <- as.factor(train[, "Survived"])
train[, "Pclass"] <- as.factor(train[, "Pclass"])
train[, "SibSp"] <- as.factor(train[, "SibSp"])
train[, "Parch"] <- as.factor(train[, "Parch"])
train = train[, !names(train) %in% c("PassengerId","Ticket","Name")]
levels(train[, "Survived"]) <- c("No", "Yes")
train[, "Cabin"] <- strtrim(train[, "Cabin"], 1)
train[, "Cabin"] <- as.factor(train[, "Cabin"])
test[, "Pclass"] <- as.factor(test[, "Pclass"])
test[, "SibSp"] <- as.factor(test[, "SibSp"])
test[, "Parch"] <- as.factor(test[, "Parch"])
test[, "Cabin"] <- strtrim(test[, "Cabin"], 1)
test[, "Cabin"] <- as.factor(test[, "Cabin"])
test = test[, !names(test) %in% c("PassengerId","Ticket","Name")]
test$Parch[test$Parch == 9] <- 6
test$Cabin[which(!(test$Cabin %in% levels(train[,"Cabin"])))] <- NA
test$Parch[which(!(test$Parch %in% levels(train[,"Parch"])))] <- NA
test$Embarked[which(!(test$Embarked %in% levels(train[,"Embarked"])))] <- NA
library(caret)
numerics <- c("Age", "Fare")
churnTrain$Cabin[which(!(test$Cabin %in% levels(train[,"Cabin"])))] <- NA
#parallelize task
library(doParallel)
registerDoParallel()
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
gbmTune <- train(Survived ~ ., data = train,
                 method = "gbm",
                 metric = "ROC",
                 verbose = FALSE,
                 trControl = ctrl)
#summary data
gbmPred2 <- predict.train(gbmTune, test, na.action = na.pass)
str(gbmPred2)
summary(gbmPred2)
#gbm
gendermodel$Survived[gbmPred2 == "No"] <- 0
gendermodel$Survived[gbmPred2 == "Yes"] <- 1
write.csv(gendermodel, file = "Testing.csv", row.names = FALSE)

#final C5.0

train <- train2
test <- test2
gendermodel <- gendermodel2
#cleaning up original data
train[, "Survived"] <- as.factor(train[, "Survived"])
train[, "Pclass"] <- as.factor(train[, "Pclass"])
train[, "SibSp"] <- as.factor(train[, "SibSp"])
train[, "Parch"] <- as.factor(train[, "Parch"])
train = train[, !names(train) %in% c("PassengerId","Ticket","Name")]
levels(train[, "Survived"]) <- c("No", "Yes")
train[, "Cabin"] <- strtrim(train[, "Cabin"], 1)
train[, "Cabin"] <- as.factor(train[, "Cabin"])
test[, "Pclass"] <- as.factor(test[, "Pclass"])
test[, "SibSp"] <- as.factor(test[, "SibSp"])
test[, "Parch"] <- as.factor(test[, "Parch"])
test[, "Cabin"] <- strtrim(test[, "Cabin"], 1)
test[, "Cabin"] <- as.factor(test[, "Cabin"])
test = test[, !names(test) %in% c("PassengerId","Ticket","Name")]
test$Parch[test$Parch == 9] <- 6
test$Cabin[which(!(test$Cabin %in% levels(train[,"Cabin"])))] <- NA
test$Parch[which(!(test$Parch %in% levels(train[,"Parch"])))] <- NA
test$Embarked[which(!(test$Embarked %in% levels(train[,"Embarked"])))] <- NA
library(caret)
train$Cabin[which(!(test$Cabin %in% levels(train[,"Cabin"])))] <- NA
#parallelize task
library(doParallel)
registerDoParallel()
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
c5Model <- train(Survived ~ ., data = train,
                 method = "C5.0",
                 metric = "ROC",
                 tuneLength = 10,
                 trControl = ctrl)
#summary data
C50Pred2 <- predict.train(c5Model, test, na.action = na.pass)
str(C50Pred2)
summary(C50Pred2)
#gbm
gendermodel$Survived[C50Pred2 == "No"] <- 0
gendermodel$Survived[C50Pred2 == "Yes"] <- 1
write.csv(gendermodel, file = "Testing.csv", row.names = FALSE)


#~~~~~~~~ new algorithm DEPRECATED

#~~~~~importing and backing up inputs

#~~ importing items
setwd("C:/Users/Laurae/Documents/Data Science/Kaggle/Titanic/")
train <- read.table("train.csv", sep = ",", header = TRUE)
test <- read.table("test.csv", sep = ",", header = TRUE)

#~~ backing up items
train2 <- train
test2 <- test
gendermodel2 <- gendermodel

#rewrite data
train <- train2
test <- test2

#~~~~ cleaning up original data

#~~ training data

#factorize
train[, "Survived"] <- as.factor(train[, "Survived"])
train[, "Pclass"] <- as.factor(train[, "Pclass"])
train[, "SibSp"] <- as.factor(train[, "SibSp"])

#defactorize
train$Name <- as.character(train$Name)

#delete useless columns
train = train[, !names(train) %in% c("PassengerId","Ticket")]

#rename level Survived
levels(train[, "Survived"]) <- c("No", "Yes")

#trimming cabin letter
train[, "Cabin"] <- strtrim(train[, "Cabin"], 1)
train[, "Cabin"] <- as.factor(train[, "Cabin"])

#cleaning up names
library(stringr)
train$Name[!is.na(str_extract(train$Name, "Mr"))] <- "Mr"
train$Name[!is.na(str_extract(train$Name, "Mrs"))] <- "Mrs"
train$Name[!is.na(str_extract(train$Name, "Mme"))] <- "Mrs"
train$Name[!is.na(str_extract(train$Name, "Miss"))] <- "Miss"
train$Name[!is.na(str_extract(train$Name, "Ms"))] <- "Miss"
train$Name[!is.na(str_extract(train$Name, "Mlle"))] <- "Miss"
train$Name[!is.na(str_extract(train$Name, "Capt"))] <- "Mr"
train$Name[!is.na(str_extract(train$Name, "Major"))] <- "Mr"
train$Name[!is.na(str_extract(train$Name, "Col"))] <- "Mr"
train$Name[!is.na(str_extract(train$Name, "Master"))] <- "Master"
train$Name[!is.na(str_extract(train$Name, "Rev"))] <- "Mr"
train$Name[!is.na(str_extract(train$Name, "Dr"))] <- "Mr"
train$Name[!is.na(str_extract(train$Name, "Don"))] <- "Mr"
train$Name[!is.na(str_extract(train$Name, "Countess"))] <- "Mrs"
train$Name[!is.na(str_extract(train$Name, "Jonkheer"))] <- "Mr"
train$Name <- factor(train$Name)

#scaling reals
library(caret)
library(caretEnsemble)
train$Parch <- as.numeric(as.character(train$Parch))
procValues <- preProcess(train[, c("Age", "Fare")], method = c("center", "scale"))
train <- predict(procValues, train)

#~~ test data

#factorize
test[, "Pclass"] <- as.factor(test[, "Pclass"])
test[, "SibSp"] <- as.factor(test[, "SibSp"])

#defactorize
test$Name <- as.character(test$Name)

#delete useless columns
test = test[, !names(test) %in% c("PassengerId","Ticket")]

#trimming cabin letter
test[, "Cabin"] <- strtrim(test[, "Cabin"], 1)
test[, "Cabin"] <- as.factor(test[, "Cabin"])

#cleaning up names
library(stringr)
test$Name[!is.na(str_extract(test$Name, "Mr"))] <- "Mr"
test$Name[!is.na(str_extract(test$Name, "Mrs"))] <- "Mrs"
test$Name[!is.na(str_extract(test$Name, "Mme"))] <- "Mrs"
test$Name[!is.na(str_extract(test$Name, "Miss"))] <- "Miss"
test$Name[!is.na(str_extract(test$Name, "Ms"))] <- "Miss"
test$Name[!is.na(str_extract(test$Name, "Mlle"))] <- "Miss"
test$Name[!is.na(str_extract(test$Name, "Capt"))] <- "Mr"
test$Name[!is.na(str_extract(test$Name, "Major"))] <- "Mr"
test$Name[!is.na(str_extract(test$Name, "Col"))] <- "Mr"
test$Name[!is.na(str_extract(test$Name, "Master"))] <- "Master"
test$Name[!is.na(str_extract(test$Name, "Rev"))] <- "Mr"
test$Name[!is.na(str_extract(test$Name, "Dr"))] <- "Mr"
test$Name[!is.na(str_extract(test$Name, "Don"))] <- "Mr"
test$Name[!is.na(str_extract(test$Name, "Countess"))] <- "Mrs"
test$Name[!is.na(str_extract(test$Name, "Jonkheer"))] <- "Mr"
test$Name <- factor(test$Name)

#scaling reals
library(caret)
library(caretEnsemble)
test$Parch <- as.numeric(as.character(test$Parch))
test <- predict(procValues, test)

#~~~~ exporting cleaning CSVs
write.csv(train, "train_clean.csv", row.names = FALSE)
write.csv(test, "test_clean.csv", row.names = FALSE)

#~~~~~~ prediction models

#~~~~ predicting against known data set

#~~ load clean CSVs
#train <- read.table("train_clean.csv", sep = ",", header = TRUE)
#test <- read.table("test_clean.csv", sep = ",", header = TRUE)
inTrainingSet <- createDataPartition(train$Survived, p = .75, list = FALSE)

#~~ or LOAD DATA
test <- train[-inTrainingSet,]
train <- train[ inTrainingSet,]

#~~ parallelize task / deprecated
library(doParallel)
registerDoParallel()

#~~ launching model

#original caret method
ctrl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE, summaryFunction = twoClassSummary)
#model_list1 <- train(Survived ~ ., data = train, method = "gbm", metric = "ROC", verbose = FALSE, trControl = ctrl)
#model_list2 <- train(Survived ~ ., data = train, method = "ada", metric = "ROC", verbose = FALSE, trControl = ctrl)

#original caretEnsemble method
ctrl <- trainControl(method = "repeatedcv", repeats = 5, savePredictions = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary, index = createMultiFolds(train$Survived, k = 5, times = 1))
model_list1 <- caretList(Survived ~ ., data = train, methodList = "gbm", metric = "ROC", verbose = FALSE, trControl = ctrl, na.action = na.pass)
model_list2 <- caretList(Survived ~ ., data = train, methodList = "ada", metric = "ROC", verbose = FALSE, trControl = ctrl, na.action = na.pass)
#model_list <- caretList(Survived ~ ., data = train, trControl = ctrl, methodList = "glmboost", na.action = na.pass)
#model_list <- c(model_list1, model_list2)
#class(model_list) <- "caretList"
#ctrl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE, index = createResample(train$Survived, 25), summaryFunction = twoClassSummary)
#model_list <- caretList(Survived ~ Pclass + Name + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked, data = train, methodList = c("gbm", "rf"), metric = "ROC", verbose = FALSE, trControl = ctrl)
model_list_temp <- c(model_list1, model_list2)
#class(model_list_temp) <- "caretList"
#rm(model_list_temp)
#model_list_temp <- NULL
#model_list_temp$gbm <- model_list1
#model_list_temp$rf <- model_list2
#class(model_list_temp) <- "caretList"
ctrl <- trainControl(method = "repeatedcv", repeats = 5, savePredictions = "final", classProbs = TRUE, summaryFunction = twoClassSummary)
#model_ensemble <- caretStack(model_list_temp, method = "glm", metric = "ROC", ctrl)
model_ensemble <- caretEnsemble(model_list_temp)
model_preds <- predict(model_ensemble, newdata = train, na.action = na.pass, type = "class")
library(caTools)


model_list_temp <- c(model_list1, model_list2)

#linear combining models
#model_ensemble <- caretEnsemble(model_list, metric="ROC", trControl=ctrl)
#summary(model_ensemble)




#predict CV
#predicted <- predict.train(model_ensemble, test, na.action = na.pass)
gbmPred <- predict.train(gbmTune, churnTest, na.action = na.pass)
str(gbmPred)
summary(gbmPred)
confusionMatrix(gbmPred, churnTest$Survived)
C50Pred <- predict.train(gbmTune, churnTest, na.action = na.pass)
str(C50Pred)
summary(C50Pred)
confusionMatrix(C50Pred, churnTest$Survived)
str(predicted)
summary(predicted)