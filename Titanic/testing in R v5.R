#~~~~~~~~ new algorithm #SuperLearner method

#~~~~ load packages

library(SuperLearner)
library(caret)
library(caretEnsemble)
library(Amelia)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(doParallel) #do not parallelize because it can creates major issues

#~~~~ clean up

remove(list = ls())

#~~~~~~~~~~~~~~ USER INPUT ~~~~~~~~~~~~~~

#~~~~ what methods?
oldinputCSV = "Testing2.csv"
newinputCSV = "Testing.csv"

#for binary input, use this when using probabilities:
#test$Survived[test$Survived >= 0.5] <- 1
#test$Survived[test$Survived < 0.5] <- 0

#~~~~~~~~~~~ END USER INPUT ~~~~~~~~~~

#~~~~~importing and backing up inputs

#~~ importing items
setwd("C:/Users/Laurae/Documents/Data Science/Kaggle/Titanic/")
train <- read.table("train.csv", sep = ",", header = TRUE)
test <- read.table("test.csv", sep = ",", header = TRUE)
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)

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
train[, "FamilySize"] <- train[, "Pclass"] + train[, "SibSp"]

#defactorize
train$Name <- as.character(train$Name)

#delete useless columns
train = train[, !names(train) %in% c("PassengerId","Ticket")]

#rename level Survived
levels(train[, "Survived"]) <- c("No", "Yes")

#trimming cabin letter
train[, "Cabin"] <- strtrim(train[, "Cabin"], 1)
train[, "Cabin"] <- as.factor(train[, "Cabin"])

#~~ test data

#factorize
test[, "FamilySize"] <- test[, "Pclass"] + test[, "SibSp"]

#defactorize
test$Name <- as.character(test$Name)

#delete useless columns
test = test[, !names(test) %in% c("PassengerId","Ticket")]

#trimming cabin letter
test[, "Cabin"] <- strtrim(test[, "Cabin"], 1)
test[, "Cabin"] <- as.factor(test[, "Cabin"])

#getting rid of characters in factors
train$Sex <- factor(train$Sex)
test$Sex <- factor(test$Sex)
train$Cabin <- factor(train$Cabin)
test$Cabin <- factor(test$Cabin)
train$Embarked <- factor(train$Embarked)
test$Embarked <- factor(test$Embarked)
train$Survived <- factor(train$Survived)

#setting up family thingies
fulldata <- join(train, test, type = "full")
fulldata[, "FamilyName"] <- sapply(fulldata$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#train[train$FamilySize > 4, "FamilyName"] <- (fulldata$FamilyName[1:891])[train$FamilySize > 4]
#test[test$FamilySize > 4, "FamilyName"] <- (fulldata$FamilyName[892:1309])[test$FamilySize > 4]
train[train$FamilySize > 4, "FamilyName"] <- "Large"
test[test$FamilySize > 4, "FamilyName"] <- "Large"
train[train$FamilySize < 5, "FamilyName"] <- "Unknown"
test[test$FamilySize < 5, "FamilyName"] <- "Unknown"
train[train$FamilyName %in% c("Backstrom", "Hansen", "Kink", "Kink-Heilmann", "Renouf"), "FamilyName"] <- "Unknown"
test[test$FamilyName %in% c("Backstrom", "Hansen", "Kink", "Kink-Heilmann", "Renouf"), "FamilyName"] <- "Unknown"
#86 (Backstrom), #861 (Hansen), #70/1268 (Kink), #1286 (Kink-Heilman), #727 (Renouf) are supposedly false positives (< 3 occ.)
train$FamilyName <- as.factor(train$FamilyName)
test$FamilyName <- as.factor(test$FamilyName)
fulldata$FamilyName <- as.factor(fulldata$FamilyName)


#cleaning up names
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
fulldata$Name[1:891] <- levels(train$Name)[train$Name]

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
fulldata$Name[892:1309] <- levels(test$Name)[test$Name]

train$Name <- as.factor(train$Name)
train$FamilyName <- as.factor(train$FamilyName)
test$Name <- as.factor(test$Name)
test$FamilyName <- as.factor(test$FamilyName)
fulldata$Name <- as.factor(fulldata$Name)


#~~ fixing missing data
fulldata$Age <- fulldata$Age / 2
fulldata$Age <- as.integer(fulldata$Age * 2)
fulldata$Name <- as.integer(fulldata$Name)
fulldata$Sex <- as.integer(fulldata$Sex)
fulldata$Cabin <- as.integer(fulldata$Cabin)
fulldata$FamilyName <- as.integer(fulldata$FamilyName)
fulldata$Embarked <- as.numeric(fulldata$Embarked)
fulldata$Embarked[c(62,830)] <- NA
NAdata <- amelia(x = fulldata, m = 1, noms = c("Embarked"), idvars = c("FamilyName", "Sex", "Survived", "FamilySize"))
train$Age[is.na(train$Age)] <- NAdata$imputations$imp1$Age[is.na(train$Age)] / 2
test$Age[is.na(test$Age)] <- NAdata$imputations$imp1$Age[is.na(test$Age)] / 2
test$Fare[is.na(test$Fare)] <- NAdata$imputations$imp1$Fare[is.na(test$Fare)]
train$Embarked[c(62,830)] <- levels(train$Embarked)[NAdata$imputations$imp1$Embarked[c(62,830)]]
train$Embarked <- factor(train$Embarked)
levels(train$Cabin)[1] <- "None"
levels(test$Cabin)[1] <- "None"
#missmap(fulldata)
#missmap(train)
#missmap(test)

#~~ scaling reals | NEW METHOD, "BETTER"
procValues <- preProcess(fulldata[, c("Age", "Fare")], method = c("center", "scale", "YeoJohnson", "nzv"), verbose = TRUE)
train <- predict(procValues, train)
test <- predict(procValues, test)

#~~ get rid of factor issues (non-existing or not useful)
train[sapply(train, is.factor)] <- lapply(train[sapply(train, is.factor)], as.character)
test[sapply(test, is.factor)] <- lapply(test[sapply(test, is.factor)], as.character)
fulldata <- rbind(train, data.frame(Survived = rep(NA, nrow(test)), test))
fulldata[sapply(fulldata, is.character)] <- lapply(fulldata[sapply(fulldata, is.character)], as.factor)
fulldata[sapply(fulldata, is.factor)] <- lapply(fulldata[sapply(fulldata, is.factor)], as.integer)
train <- fulldata[c(1:891),]
test <- fulldata[c(892:1309), -1]

#~~~~ exporting cleaning CSVs
#write.csv(train, "train_clean.csv", row.names = FALSE)
#write.csv(test, "test_clean.csv", row.names = FALSE)
train2 <- train
test2 <- test


#~~~~~~~~~~ PREDICTIONS

#generalized linear model (glm)
#linear discriminant analysis (lda)
#regularized regression (glmnet)
#k-nearest neighbors (knn)
#naive bayes (nb)
#CART (rpart)

#~wrapper to create caret models for SuperLearner

SL.caret2 <- function(Y, X, newX, family, obsWeights, method = "rf", trControl = caret::trainControl(method = "cv", number = 5, classProbs = TRUE), metric = ifelse(family$family == 'gaussian', 'RMSE', 'Accuracy'), ...) {
  if (family$family == "gaussian") {
    fit.train <- caret::train(x = X, y = Y, weights = obsWeights, metric = metric, method = method, tuneLength = tuneLength, trControl = trControl)
    pred <- predict(fit.train, newdata = newX, type = "raw")
  }
  if (family$family == "binomial") {
    # outcome must be factor, and have real labels
    Y.f <- as.factor(Y)
    levels(Y.f) <- c("A0", "A1")
    fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights, metric = metric, method = method, trControl = trControl)
    pred <- predict(fit.train, newdata = newX, type = "prob")[, 2]
  }
  fit <- list(object = fit.train)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.caret")
  return(out)
}

SL.caret2t <- function(Y, X, newX, family, obsWeights, method = "rf", tuneGrid = data.frame(Nothing = c(NA)), trControl = caret::trainControl(method = "cv", number = 5, classProbs = TRUE), metric = ifelse(family$family == 'gaussian', 'RMSE', 'Accuracy'), ...) {
  if (family$family == "gaussian") {
    fit.train <- caret::train(x = X, y = Y, weights = obsWeights, metric = metric, method = method, trControl = trControl, tuneGrid = tuneGrid)
    pred <- predict(fit.train, newdata = newX, type = "raw")
  }
  if (family$family == "binomial") {
    # outcome must be factor, and have real labels
    Y.f <- as.factor(Y)
    levels(Y.f) <- c("A0", "A1")
    fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights, metric = metric, method = method, trControl = trControl, tuneGrid = tuneGrid)
    pred <- predict(fit.train, newdata = newX, type = "prob")[, 2]
  }
  fit <- list(object = fit.train)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.caret")
  return(out)
}


#modelList = c("glm", "lda", "glmnet", "knn", "nb", "rpart")
#modelList = c("cforest", "rpart", "rpart2", "rpart1SE", "xgbTree", "C5.0", "ctree", "ctree2", "blackboost")
#modelList = c("cforest", "parRF", "rf", "extraTrees", "Boruta", "RRF", "wsrf") #random forests
modelList = c("cforest", "rf", "wsrf", "Boruta") #best forest models
#modelList = c("AdaBoost.M1", "AdaBag", "treebag", "LogitBoost", "blackboost", "C5.0", "rpart", "ctree", "gbm", "evtree", "nodeHarvest") #tree based models
create_caret_model <- function(kn) {for (mm in kn) {
  eval(parse(text = paste("model.", mm, " <<- function(..., method = '", mm, "') SL.caret2(..., method = method, metric = 'ROC')", sep = "")))
}}
create_caret_model(kn = modelList)
#model.cforest4 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 4))
#model.cforest5 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 5))
#model.cforest6 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 6))
#model.cforest7 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 7))
#model.cforest8 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 8))
#model.cforest9 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 9))
#model.cforest10 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 10))
#model.cforest11 <- function(..., method = "cforest") SL.caret2t(..., method = method, tuneGrid = data.frame(mtry = 11))

SL.library <- c(paste("model.", modelList, sep = ""))

Y <- as.numeric(train$Survived) - 1 #avoid using subset because we need a value and not a data frame, must be binary (0 or 1) ONLY
X <- subset(train, select = -Survived)


#~~ SuperLearner meta model of crossvalidated models

cl <- makeCluster(2)
registerDoParallel()
fitSL <- SuperLearner(Y = Y, X = X,
                      SL.library = SL.library,
                      family = binomial(),
                      method = "method.AUC",
                      control = list(saveFitLibrary = TRUE),
                      cvControl = list(V = 5, stratifyCV = TRUE),
                      verbose = TRUE)
#fitSL <- SuperLearner(Y = Y, X = X, SL.library = c(paste("model.cforest", 4:11, sep = "")), family = binomial(), method = "method.AUC", control = list(saveFitLibrary = TRUE), cvControl = list(V = 5, stratifyCV = TRUE), verbose = TRUE)
stopCluster(cl)
registerDoSEQ()
closeAllConnections()
fitSL
predictedValues <- predict.SuperLearner(fitSL, newdata = test, X = X, Y = Y)

#methods: method.NNLS, method.NNLS2, method.NNloglik, method.CC_nloglik, method.AUC
fitSLreco <- recombineSL(fitSL, Y = Y, method = "method.NNloglik", verbose = TRUE)
fitSLreco
predictedValues <- predict.SuperLearner(fitSLreco, newdata = test, X = X, Y = Y)



#~~ output data to CSV

gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
gendermodel$Survived <- as.numeric(predictedValues$pred)
gendermodel$Survived[predictedValues$pred < 0.5] <- 0
gendermodel$Survived[predictedValues$pred >= 0.5] <- 1
oldgendermodel <- read.table(oldinputCSV, sep = ",", header = TRUE)
#oldgendermodel <- read.table(newinputCSV, sep = ",", header = TRUE)
print("Difference of predictions: Not different")
print(table(oldgendermodel$Survived == gendermodel$Survived))
write.csv(gendermodel, file = newinputCSV, row.names = FALSE)
