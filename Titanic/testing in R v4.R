#~~~~~~~~ new algorithm #SuperLearner method

#~~~~ load packages

library(SuperLearner)
library(subsemble)
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

#creating random forest models
tuneGrid <- expand.grid(mtry = c(500, 1000), nodesize = c(1, 3, 5))
for (mm in seq(nrow(tuneGrid))) {
  eval(parse(text = paste("SL.randomForest.", mm,
                   " <- function(..., mtry = ", tuneGrid[mm, 1],
                   ", nodesize = ", tuneGrid[mm, 2], ") {
                   SL.randomForest(..., mtry = mtry,
                   nodesize = nodesize) } ", sep = "")))
}

create.SL.knn <- function(kn) {for (mm in kn) {
  eval(parse(text = paste("SL.knn.", mm, " <<- function(..., k = ", mm, ") SL.knn(..., k = k)", sep = "")))
}}

#create.SL.knn(kn = c(20, 30, 40, 50))

create.SL.glmnet <- function(alphan) {for (mm in alphan) {
  eval(parse(text = paste("SL.glmnet.", mm, " <<- function(..., alpha = ", mm, ") SL.glmnet(..., alpha = alpha)", sep = "")))
}}

create.SL.glmnet(alphan = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

SL.library <- c(paste("SL.glmnet.", c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), sep = ""),
                paste("SL.randomForest.", seq(nrow(tuneGrid)), sep = ""))

Y = as.numeric(train$Survived) - 1 #avoid using subset because we need a value and not a data frame, must be binary (0 or 1) ONLY
X = subset(train, select = -Survived)


#~~ Crossvalidated SuperLearner meta model of crossvalideted models

cl <- makeCluster(2)
registerDoParallel()
fitSL.CV <- CV.SuperLearner(Y = Y, X = X,
                            SL.library = SL.library,
                            family = binomial(),
                            V = 5,
                            method = "method.AUC",
                            control = list(saveFitLibrary = TRUE),
                            cvControl = list(V = 5, stratifyCV = TRUE),
                            verbose = TRUE,
                            saveAll = TRUE,
                            ) #parallel = "multicore" / cl, not working
stopCluster(cl)
registerDoSEQ()
closeAllConnections()
summary(fitSL.CV)
plot.CV.SuperLearner(fitSL.CV)
predictedValues <- predict.SuperLearner(fitSL.CV, newdata = test, X = X, Y = Y)


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
stopCluster(cl)
registerDoSEQ()
closeAllConnections()
fitSL
predictedValues <- predict.SuperLearner(fitSL, newdata = test, X = X, Y = Y)


#~~ Crossvalidated SuperLearner meta model of crossvalidated models using Subsemple - not a good idea

cl <- makeCluster(2)
registerDoParallel()
fitSL <- subsemble(x = X, y = Y,
                   family = binomial(),
                   learner = SL.library,
                   metalearner = "SL.glm",
                   subsets = 1,
                   cvControl = list(V = 5, stratifyCV = TRUE))  #learnControl = list(multiType = "divisor") = for fast if subsets > 1, requires length(learner) dividing number of subsets
stopCluster(cl)
registerDoSEQ()
closeAllConnections()
auc <- AUC(predictions = fitSL$pred, labels = Y)
print(AUC)
predictedValues <- predict(fitSL, newx = test)



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
