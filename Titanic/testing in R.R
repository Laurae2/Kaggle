#~~~~~~~~ new algorithm

remove(list = ls())

#~~~~~~~~~~~~~~ USER INPUT ~~~~~~~~~~~~~~

#~~~~ what methods?
methodUsed <- c("gbm", "C5.0", "bstTree", "glmboost")
preProcessing <- c("center", "scale", "YeoJohnson", "nzv")
oldinputCSV = "Testing.csv"
newinputCSV = "Testing2.csv"

#~~~~ preprocessing methods:
#center = mean reduction (any value)
#scale = standard deviation division (any value)
#range = scales data to interval [0,1]
#knnImpute = k-nearest neightbor imputation using Euclidian distance
#bagImpute = bagging imputation
#medianImpute = impute medians on missing values (inaccurate)
#pca = principal component analysis (forces center+scale, also renames column names to PC1, PC2...)
#ica = independent component analysis (forces center+scale, also renames column names to PC1, PC2...)
#spatialSign = spatial sign transformation (center+scale data projection in circle with {p=number of predictors} circles)
#ignore = CANNOT be used.
#keep = CANNOT be used.
#remove = CANNOT Be used.
#BoxCox = power transformation (strict positivity)
#YeoJohnson = BoxCox + allows zero/negative values (any value)
#expoTrans = exponential transformation (any value)
#zv = finds zero variance variables and excludes them from calculations
#nzv = zv + near zero variance (excludes near zero-variance predictors)
#conditionalX = unique(x sparse to y) values are excluded (not applicable on non-factors)
#
#order it is done: zv, nzf,  box-cox/yeo-johnson/exponential trans, centering, scaling, range, imputation, pca, ica, spatial sign

#~~~~~ for each index, what columns?
col1 <- c("Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked")
col2 <- c("Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked")
col3 <- c("Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare")
col4 <- c("Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked")

#~~~~~ what to columns transform into indexes+1 (integers)?
bin1 <- NA
bin2 <- NA
bin3 <- c(3,4,5,8)
bin4 <- c(3,4,5,8,9,10)

#modelLookup()[modelLookup()[,"probModel"] == TRUE,][, c("model","label")]

#most common methods: gbm, ada, logitBoost, rpart, glmboost

#use this to find classProb: modelLookup()[modelLookup()[,"probModel"] == TRUE,]$model
#forClass: modelLookup()[modelLookup()[,"forClass"] == TRUE,]$model
#forReg: modelLookup()[modelLookup()[,"forReg"] == TRUE,]$model

#many methods
#ada = boosted classification trees
#AdaBag = bagged AdaBoost
#AdaBoost.M1 = AdaBoost.M1
#amdai = Adaptive Mixture Discriminant Analysis
#avNNet = Model Averaged Neural Network
#awtan = Tree Augmented Naive Bayes Classifier with Attribute Weighting
#bag = Bagged Model
#bagFDA = Bagged Flexible Discriminant Analysis
#bartMachine = Bayesian Additive Regression Trees
#blackboost = Boosted Tree
#bmn = Bayesian Regularized Neural Networks
#Boruta = Random Forest with Additional Feature Selection
#bstTree = Boosted Tree
#C5.0 = C5.0
#cforest = Conditional Inference Random Forest
#chaid = CHAID
#ctree = Conditional Inference Tree
#DENFIS = Dynamic Evolving Neural-Fuzzy Inference System
#dnn = Stacked AutoEncoder Deep Neural Network
#evtree = Tree Models from Genetic Algorithms
#extraTrees = Random Forest by Randomization
#HYFIS = Hybrid Neural Fuzzy Inference System
#gamboost = Boosted Generalized Additive Model
#gbm = Stochastic Gradient Boosting
#glmboost = Boosted Generalized Linear Model
#LMT = Logistic Model Trees
#logicBag = Bagged Logic Regression
#logitBoost = Boosted Logistic Regression
#M5 = Model Tree
#nnet = Neural Network
#nodeHarvest = Tree-Based Ensembles
#pcaNNet = Neural Networks with Feature Extraction
#qrf = Quantile Random Forest
#qrnn = Quantile Regression Neural Network
#rf = Random Forest
#rfRules = Random Forest Rule-Based Model
#rpart = CART
#RRF = Regularized Random Forest
#tan = Tree Augmented Naive Bayes Classifier
#tanSearch = Tree Augmented Naive Bayes Classifier Structure Learner Wrapper
#treebag = Bagged CART
#wsrf = Weighted Subspace Random Forest
#xgbLinear = eXtreme Gradient Boosting
#xgbTree = eXtreme Gradient Boosting

#for binary input, use this when using probabilities:
#test$Survived[test$Survived >= 0.5] <- 1
#test$Survived[test$Survived < 0.5] <- 0

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

#~~~~ scaling sets
library(caret)

#~~ fixing missing data
library(Amelia)
library(plyr)
train$Pclass <- as.numeric(levels(train$Pclass)[train$Pclass])
train$SibSp <- as.numeric(levels(train$SibSp)[train$SibSp])
test$Pclass <- as.numeric(levels(test$Pclass)[test$Pclass])
test$SibSp <- as.numeric(levels(test$SibSp)[test$SibSp])
fulldata <- join(test, train, type = "full")
fulldata[, "Name"] <- as.numeric(fulldata[, "Name"])
fulldata[, "Sex"] <- as.numeric(fulldata[, "Sex"])
fulldata[, "Cabin"] <- as.numeric(fulldata[, "Cabin"])
fulldata[, "Embarked"] <- as.numeric(fulldata[, "Embarked"])
fulldata[, "Survived"] <- as.numeric(fulldata[, "Survived"])
NAdata <- amelia(x = fulldata, m = 1)
train$Age[is.na(train$Age)] <- NAdata$imputations$imp1$Age[is.na(train$Age)]
test$Age[is.na(test$Age)] <- NAdata$imputations$imp1$Age[is.na(test$Age)]
test$Fare[is.na(test$Fare)] <- NAdata$imputations$imp1$Fare[is.na(test$Fare)]
levels(train$Cabin)[1] <- "None"
levels(test$Cabin)[1] <- "None"
#missmap(fulldata)
#missmap(train)
#missmap(test)

#~~ GLM model to determine missing age/fare using all data - deprecated method
#fulldata <- join(test, train, type = "full")
#age.pre <- glm(Age ~ Pclass + Sex + Parch + Fare, data = fulldata)
#fare.pre <- glm(Fare ~ Pclass + Sex + Parch + Age, data = fulldata)
#train$Age[is.na(train$Age)] <- predict(age.pre, train)[is.na(train$Age)]
#test$Age[is.na(test$Age)] <- predict(age.pre, test)[is.na(test$Age)]
#test$Fare[is.na(test$Fare)] <- predict(fare.pre, test)[is.na(test$Fare)]

#~~ supposed embarked for the two people with missing data
train$Embarked[train$Embarked == ""] <- "S"
train$Embarked <- factor(train$Embarked)

#~~ scaling reals | OLD METHOD, DEPRECATED
train$Parch <- as.numeric(as.character(train$Parch))
#procValues <- preProcess(train[, c("Age", "Fare")], method = c("center", "scale"))
#train <- predict(procValues, train)
test$Parch <- as.numeric(as.character(test$Parch))
#test <- predict(procValues, test)

#~~ scaling reals | NEW METHOD, "BETTER"
procValues <- preProcess(fulldata[, c("Age", "Fare")], method = preProcessing, verbose = TRUE)
train <- predict(procValues, train)
test <- predict(procValues, test)

#~~~~ exporting cleaning CSVs
write.csv(train, "train_clean.csv", row.names = FALSE)
write.csv(test, "test_clean.csv", row.names = FALSE)
train2 <- train
test2 <- test






#~~~~~~ prediction models / using caret alone, no ensembling methods

#~~~~ predicting against known data set

#~~ load clean CSVs
train <- train2
test <- test2
#train <- read.table("train_clean.csv", sep = ",", header = TRUE)
#test <- read.table("test_clean.csv", sep = ",", header = TRUE)
inTrainingSet <- createDataPartition(train$Survived, p = .8, list = FALSE)

#~~ or LOAD DATA
test <- train[-inTrainingSet,]
train <- train[ inTrainingSet,]

#~~ parallelize task
#library(doParallel)

#~~ launching model

#caret method
ctrl <- trainControl(method = "adaptive_cv", repeats = 3, number = 5, savePredictions = TRUE, classProbs = TRUE, index = createMultiFolds(train$Survived, 5, 3))
for(i in 1:length(methodUsed)){
  print(paste("Doing ", methodUsed[i], "(", i, ")", sep = ""))
  tempVarName1 <- paste("model_list", i, sep = "")
  tempVarName2 <- paste("col", i, sep = "")
  tempVarName3 <- paste("bin", i, sep = "")
  train_temp <- train[, c("Survived", get(tempVarName2))]
  if (!is.na(get(tempVarName3))[1]){
    for(Vars in get(tempVarName3)){
      train_temp[, Vars] <- as.numeric(train_temp[, Vars])
      train_temp[, Vars] <- train_temp[, Vars] - min(train_temp[, Vars]) + 1
    }
  }
  #cl <- makeCluster(2)
  #registerDoParallel()
  assign(tempVarName1, train(Survived ~ ., data = train_temp, method = methodUsed[i], trControl = ctrl))
  #stopCluster(cl)
}

#confusion matrix
for(i in 1:length(methodUsed)){
  print(paste("Doing ", methodUsed[i], "(", i, ")", sep = ""))
  tempVarName <- paste("model_list", i, sep = "")
  tempVarName2 <- paste("col", i, sep = "")
  tempVarName3 <- paste("bin", i, sep = "")
  test_temp <- test[, c("Survived", get(tempVarName2))]
  if (!is.na(get(tempVarName3))[1]){
    for(Vars in get(tempVarName3)){
      test_temp[, Vars] <- as.numeric(test_temp[, Vars])
      test_temp[, Vars] <- test_temp[, Vars] - min(test_temp[, Vars]) + 1
    }
  }
  Pred <- predict.train(get(tempVarName), test_temp)
  tempVarName <- paste("confMatrix", i, sep = "")
  assign(tempVarName, confusionMatrix(Pred, test$Survived))
}
#for(i in 1:length(methodUsed)){
#  tempVarName <- paste("confMatrix", i, sep = "")
#  get(tempVarName)
#}

#parse confusion matrices data
confMatrices <- as.matrix(confMatrix1$overall)
confMatrices <- rbind(confMatrices, as.matrix(confMatrix1$byClass))
if (length(methodUsed) != 1) {
  for(i in 2:length(methodUsed)) {
    tempVarName <- paste("confMatrix", i, sep = "")
    confMatrices <- cbind(confMatrices, rbind(as.matrix(get(tempVarName)$overall), as.matrix(get(tempVarName)$byClass)))
  }
}
#confMatrices = as.matrix(confMatrices[-6,])
confMatrices = as.matrix(confMatrices[-grep("PValue", rownames(confMatrices)), ])
colnames(confMatrices) <- methodUsed
print(confMatrices, digits = 4)

#ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
#model_list <- train(Survived ~ ., data = train, method = methodUsed, verbose = FALSE, trControl = ctrl, na.action = na.pass)

#predict for cross validation on unknown data set
#Pred <- predict.train(model_list1, test, na.action = na.pass)
#str(Pred)
#summary(Pred)
#confusionMatrix(Pred, test$Survived)

#real prediction but on all data set
i <- which(confMatrices == max(confMatrices["Accuracy",]), arr.ind = TRUE)[1,2]
print(paste("Best method found is: ", methodUsed[i], ", with Accuracy = ", round(max(confMatrices["Accuracy",]), 4), sep = ""))
tempVarName2 <- paste("col", i, sep = "")
tempVarName3 <- paste("bin", i, sep = "")
train_temp <- train2[, c("Survived", get(tempVarName2))]
test_temp <- test2[, c(get(tempVarName2))]
if (!is.na(get(tempVarName3))[1]){
  for(Vars in get(tempVarName3)){
    train_temp[, Vars] <- as.numeric(train_temp[, Vars])
    train_temp[, Vars] <- train_temp[, Vars] - min(train_temp[, Vars]) + 1
    test_temp[, Vars - 1] <- as.numeric(test_temp[, Vars - 1])
    test_temp[, Vars - 1] <- test_temp[, Vars - 1] - min(test_temp[, Vars - 1]) + 1
  }
}
#cl <- makeCluster(2)
#registerDoParallel()
#model_list <- train(Survived ~ ., data = train_temp, method = methodUsed[i], trControl = ctrl) #deprecated method
tempVarName <- paste("model_list", i, sep = "")
model_list <- get(tempVarName)
#stopCluster(cl)
predictedValues <- predict.train(model_list, test_temp, type = "class")
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
gendermodel$Survived[predictedValues == "No"] <- 0
gendermodel$Survived[predictedValues == "Yes"] <- 1
write.csv(gendermodel, file = "Testing.csv", row.names = FALSE)








#~~~~ another predictive usage possibility using caretEnsemble, more accurate very quickly
library(caretEnsemble)

ctrl <- trainControl(method = "adaptive_cv", repeats = 3, number = 5, savePredictions = "final", classProbs = TRUE, index = createMultiFolds(train$Survived, 5, 3))
multimodel <- caretList(Survived ~ ., data = train, trControl = ctrl, methodList = c("ada", "rpart", "xgbTree", "gbm", "C5.0", "glmboost"))
#ctrlmulti <- trainControl(number = 2, classProbs = TRUE, summaryFunction = twoClassSummary)
multiensemble <- caretEnsemble(multimodel)
summary(multiensemble)
predictedValues <- predict(multiensemble, newdata = test)
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
gendermodel$Survived[predictedValues < 0.50] <- 0
gendermodel$Survived[predictedValues >= 0.50] <- 1
oldgendermodel <- read.table(oldinputCSV, sep = ",", header = TRUE)
print("Difference of predictions:")
print(count(oldgendermodel$Survived == gendermodel$Survived))
write.csv(gendermodel, file = newinputCSV, row.names = FALSE)
