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

#~~~~ exporting cleaning CSVs
#write.csv(train, "train_clean.csv", row.names = FALSE)
#write.csv(test, "test_clean.csv", row.names = FALSE)
train2 <- train
test2 <- test


#prediction
