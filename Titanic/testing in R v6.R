#~~~~~~~~ new algorithm

#~~~~ load packages

library(caret)
library(caretEnsemble)
library(Amelia)
library(stringr)
library(plyr)
library(dplyr)
library(tidyr)
library(doParallel) #do not parallelize because it can creates major issues
library(corrplot)
library(factoextra)
library(FactoMineR)
library(gridBase)

#~~~~ clean up

remove(list = ls())

#~~~~~~~~~~~~~~ USER INPUT ~~~~~~~~~~~~~~

#~~~~ what methods?
seedingValue = 11111 #ensures reproductability
#methodUsed <- c("cforest", "rf", "rpart", "rpart2", "xgbTree", "gbm", "C5.0", "glmboost")
methodUsed <- c("cforest", "xgbTree", "C5.0")
targetUsed = "ROC" #Accuracy when N>>P, Balanced Accuracy when P>>N, Kappa when unbalanced classes, ROC when dealing with general purpose binary classification
preProcessing <- c("center", "scale", "YeoJohnson", "nzv")
oldinputCSV = "Testing2.csv"
newinputCSV = "Testing.csv"
samplingMethod = "repeatedcv" #adaptive_cv is good and faster, repeatedcv is best (caretEnsemble) and reliable ($besttune error)

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

#~~~~~ for each model, what's the specs?

#~~ how many tunings per model? (mutliplicated by CV folds x repeats x number of parameters of the specific model)
tune <- c(0)
tune[1] <- 8
tune[2] <- 10
tune[3] <- 10
#tune[1] <- 3; tune[2] <- 4; tune[3] <- 40; tune[4] <- 40; tune[5] <- 4; tune[6] <- 8; tune[7] <- 20; tune[8] <- 7
print(cbind(methodUsed, tune))
ensembleTune <- 10

specs = c("0")
for(i in 1:length(methodUsed)){
  specs[i] <- paste(methodUsed[i], " = caretModelSpec(method = \"", methodUsed[i], "\", tuneLength = ", tune[i], ")", sep = "")
}

randomness = c(0)
for(i in 1:length(methodUsed)){
  randomness[i] <- FALSE #recommended
}

#~~~~~ for each index, what columns? (single tuning)
#col = c(0) #deprecated, not working
for(i in 1:length(methodUsed)){
  tempVarName1 = paste("col", i, sep = "")
  assign(tempVarName1, c("Pclass", "Name", "Sex", "Age", "Fare", "Cabin", "Embarked", "FamilySize", "FamilyName"))
  #assign(tempVarName1, c("Pclass", "Name", "SibSp", "Parch", "Sex", "Age", "Fare", "Cabin", "Embarked", "FamilySize", "FamilyName"))
  #assign(tempVarName1, c("Pclass", "Name", "Sex", "Age", "Fare", "Cabin", "Embarked", "FamilySize", "FamilyName"))
}

#col1 <- c("Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked")
#col2 <- col1
#col3 <- col1
#col4 <- col1
#col5 <- col1

#~~~~~ what to columns transform into indexes+1 (integers)? (single tuning)
bin = c(0)
for(i in 1:length(methodUsed)){
  bin[i] <- NA
}

#bin1 <- NA
#bin2 <- NA
#bin3 <- NA
#bin4 <- NA
#bin5 <- NA

#~~~~~ how many times to run Cross Validation?
CVrepeats = 3
CVfolds = 5

#modelLookup()[modelLookup()[,"probModel"] == TRUE,][, c("model","label")]

#most common methods: gbm, ada, logitBoost, rpart, glmboost

#use this to find classProb: modelLookup()[modelLookup()[,"probModel"] == TRUE,]$model
#forClass: modelLookup()[modelLookup()[,"forClass"] == TRUE,]$model
#forReg: modelLookup()[modelLookup()[,"forReg"] == TRUE,]$model

#best and fastest models with Prob...?
#see: https://github.com/tobigithub/caret-machine-learning/blob/master/caret-classification/caret-all-binary-class-PimaIndiansDiabetes.csv
#c("parRF", "rf", "RRFglobal", "treebag", "xgbTree", "gbm", "C5.0", "gamLoess", "earth", "gcvEarth", "knn", "gamboost", "glmnet", "bayesglm", "gamspline", "glmboost", "rpart2", "svmLinear", "ctree", "blackboost", "C5.0Tree", "C5.0Rules", "nnet")
#tune <- c(5, 5, 3, 0, 3, 15, 5, 15, 15, 25, 10, 5, 10, 0, 25, 15, 50, 50, 40, 5, 0, 0, 10)
#parRF: Parallel Random Forest (7.99, 1 param)
#rf = Random Forest (7.02, 1 param)
#RRFglobal = Regularized Random Forest (7.90, 2 param)
#treebag = Bagged CART (2.48, 0 param)
#xgbTree = eXtreme Gradient Boosting (6.54, 2 param)
#gbm = Stochastic Gradient Boosting (2.15, 4 param)
#C5.0 = C5.0 (5.16, 3 param)
#gamLoess = Generalized Additive Model using LOESS (1.85, 2 param)
#earth = Multivariate Adaptive Regression Spline (1.51, 2 param)
#gcvEarth = Multivariate Adaptive Regression Splines (1.16, 1 param)
#knn = k-Nearest Neighbors (2.72, 1 param)
#gamboost = Boosted Generalized Additive Model (4.07, 2 param)
#glmnet = glmnet (1.42, 2 param)
#bayesglm = Bayesian Generalized Linear Model (1.45, 0 param)
#gamSpline = Generalized Additive Model using Splines (2.46, 1 param)
#glmboost = Boosted Generalized Linear Model (1.29, 2 param)
#rpart2 = CART (0.89, 1 param)
#svmLinear = Support Vector Machines with Linear Kernel (1.09, 1 param)
#ctree = Conditional Inference Tree (1.37, 1 param)
#blackboost = Boosted Tree (6.76, 2 param)
#C5.0Tree = Single C5.0 Tree (0.84, 0 param)
#C5.0Rules = Single C5.0 Ruleset (0.86, 0 param)
#nnet = Neural Network (3.12, 2 param)

#randomness is not working (well) for the following models and must be turned off: ada, AdaBag,
#AdaBoost.M1, bagEarth, blackboost, blasso, BstLm, bstSm, bstTree, C5.0, C5.0Cost, cubist, earth
#enet, foba, gamboost, gbm, glmboost, glmnet, kernelpls, lars, lars2, lasso, lda2, leapBackward
#leapForward, leapSeq, LogitBoost, pam, partDSA, pcr, PenalizedLDA, pls, relaxo, rfRules
#rotationForest, rotationForestCp, rpart, rpart2, rpartCost, simpls, spikeslab, superpc, widekernelpls, xgbTree

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
train$Survived <- as.factor(train$Survived)
levels(train$Survived) <- c("No", "Yes")

#~~~~ exporting cleaning CSVs
#write.csv(train, "train_clean.csv", row.names = FALSE)
#write.csv(test, "test_clean.csv", row.names = FALSE)
train2 <- train
test2 <- test






#~~~~~~ prediction models / using caret alone, no ensembling methods

#~~~~ predicting against known data set

#~~ load clean CSVs
train <- train2
test <- test2
#train <- read.table("train_clean.csv", sep = ",", header = TRUE)
#test <- read.table("test_clean.csv", sep = ",", header = TRUE)
#inTrainingSet <- createDataPartition(train$Survived, p = .8, list = FALSE)

#~~ or LOAD DATA
#test <- train[-inTrainingSet,]
#train <- train[ inTrainingSet,]

#~~ launching model

#same sampling strategy
indexPreds <- createMultiFolds(train$Survived, CVfolds, CVrepeats)

#caret method
for(i in 1:length(methodUsed)){
  print(paste("Doing ", methodUsed[i], "(", i, ")", sep = ""))
  tempVarName1 <- paste("model_list", i, sep = "")
  tempVarName2 <- paste("col", i, sep = "")
  tempVarName3 <- "bin"
  train_temp <- train[, c("Survived", get(tempVarName2))]
  if (!is.na(get(tempVarName3)[i])[1]){
    for(Vars in get(tempVarName3)[i]){
      train_temp[, Vars] <- as.numeric(train_temp[, Vars])
      train_temp[, Vars] <- train_temp[, Vars] - min(train_temp[, Vars]) + 1
    }
  }
  cl <- makeCluster(2)
  registerDoParallel()
  if(randomness[i] == 1) {
    ctrl <- trainControl(method = samplingMethod, repeats = CVrepeats, number = CVfolds, verboseIter = TRUE, returnResamp = "all", savePredictions = "all", classProbs = TRUE, summaryFunction = twoClassSummary, index = indexPreds, search = "random")
  } else {
    ctrl <- trainControl(method = samplingMethod, repeats = CVrepeats, number = CVfolds, verboseIter = TRUE, returnResamp = "all", savePredictions = "all", classProbs = TRUE, summaryFunction = twoClassSummary, index = indexPreds)
  }
  set.seed(seedingValue) #ensures reproductability
  assign(tempVarName1, train(Survived ~ ., data = train_temp, method = methodUsed[i], metric = targetUsed, trControl = ctrl, tuneLength = tune[i]))
  stopCluster(cl)
  registerDoSEQ()
  closeAllConnections()
}

#adjust best for optimization
for(i in 1:length(methodUsed)){
  print(paste("Adjusting ", methodUsed[i], "(", i, ")", sep = ""))
  tempVarName1 <- paste("model_list", i, sep = "")
  tempVarName2 <- paste("col", i, sep = "")
  tempVarName3 <- "bin"
  train_temp <- train[, c("Survived", get(tempVarName2))]
  if (!is.na(get(tempVarName3)[i])[1]){
    for(Vars in get(tempVarName3)[i]){
      train_temp[, Vars] <- as.numeric(train_temp[, Vars])
      train_temp[, Vars] <- train_temp[, Vars] - min(train_temp[, Vars]) + 1
    }
  }
  tempVarName <- as.data.frame(get(tempVarName1)$results[best(get(tempVarName1)$results, metric = "ROC", maximize = TRUE),1:ncol(get(tempVarName1)$bestTune)])
  if (ncol(tempVarName) == 1){
    colnames(tempVarName) <- names(get(tempVarName1)$bestTune)
  }
  print(get(tempVarName1))
  print("Best was:")
  print(tempVarName)
  tempVarName1 <- paste("model_list_opt", i, sep = "")
  cl <- makeCluster(2)
  registerDoParallel()
  ctrl <- trainControl(method = "repeatedcv", repeats = CVrepeats, number = CVfolds, verboseIter = TRUE, returnResamp = "all", savePredictions = "all", classProbs = TRUE, summaryFunction = twoClassSummary, index = indexPreds)
  set.seed(seedingValue) #ensures reproductability
  assign(tempVarName1, train(Survived ~ ., data = train_temp, method = methodUsed[i], metric = targetUsed, trControl = ctrl, tuneGrid = tempVarName))
  stopCluster(cl)
  registerDoSEQ()
  closeAllConnections()
}

#for(i in 1:length(methodUsed)){
#  tempVarName <- paste("confMatrix", i, sep = "")
#  get(tempVarName)
#}

#confusion matrix
for(i in 1:length(methodUsed)){
  print(paste("Doing ", methodUsed[i], "(", i, ")", sep = ""))
  tempVarName <- paste("model_list_opt", i, sep = "")
  tempVarName2 <- paste("col", i, sep = "")
  tempVarName3 <- "bin"
  test_temp <- train[, c("Survived", get(tempVarName2))]
  if (!is.na(get(tempVarName3)[i])[1]){
    for(Vars in get(tempVarName3)[i]){
      test_temp[, Vars] <- as.numeric(test_temp[, Vars])
      test_temp[, Vars] <- test_temp[, Vars] - min(test_temp[, Vars]) + 1
    }
  }
  Pred <- predict.train(get(tempVarName), test_temp)
  tempVarName <- paste("confMatrix", i, sep = "")
  assign(tempVarName, confusionMatrix(table(factor(Pred, union(Pred, test_temp$Survived)), factor(test_temp$Survived, union(Pred, test_temp$Survived)))))
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
tempVarName3 <- "bin"
train_temp <- train2[, c("Survived", get(tempVarName2))]
test_temp <- test2[, c(get(tempVarName2))]
if (!is.na(get(tempVarName3)[i])[1]){
  for(Vars in get(tempVarName3)[i]){
    train_temp[, Vars] <- as.numeric(train_temp[, Vars])
    train_temp[, Vars] <- train_temp[, Vars] - min(train_temp[, Vars]) + 1
    test_temp[, Vars - 1] <- as.numeric(test_temp[, Vars - 1])
    test_temp[, Vars - 1] <- test_temp[, Vars - 1] - min(test_temp[, Vars - 1]) + 1
  }
}
#cl <- makeCluster(2)
#registerDoParallel()
#model_list <- train(Survived ~ ., data = train_temp, method = methodUsed[i], trControl = ctrl) #deprecated method
tempVarName <- paste("model_list_opt", i, sep = "")
model_list <- get(tempVarName)
#stopCluster(cl)
predictedValues <- predict.train(model_list, test_temp, type = "raw")
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
gendermodel$Survived <- as.numeric(predictedValues) - 1
#gendermodel$Survived[predictedValues == "No"] <- 0
#gendermodel$Survived[predictedValues == "Yes"] <- 1
oldgendermodel <- read.table(oldinputCSV, sep = ",", header = TRUE)
#oldgendermodel <- read.table(newinputCSV, sep = ",", header = TRUE)
print("Difference of predictions: Not different")
print(table(oldgendermodel$Survived == gendermodel$Survived))
write.csv(gendermodel, file = newinputCSV, row.names = FALSE)






#~~~~ another predictive usage possibility using caretEnsemble, more accurate very quickly

#~~~~~~~~~~ if you have models NOT separeted, do this ~~~~~~~~~~~~

indexPreds <- createMultiFolds(train$Survived, CVfolds, CVrepeats)

if (length(randomness[randomness == 0]) == 0) {
  ctrl <- trainControl(method = samplingMethod, repeats = CVrepeats, number = CVfolds, savePredictions = "final", classProbs = TRUE, index = indexPreds, search = "random")
} else {
  ctrl <- trainControl(method = samplingMethod, repeats = CVrepeats, number = CVfolds, savePredictions = "final", classProbs = TRUE, index = indexPreds)
}
tempVarName = paste("list(", specs[1], sep = "")
if (length(methodUsed) != 1) {
  for(i in 2:length(methodUsed)) {
    tempVarName <- paste(tempVarName, ", ", specs[i], sep = "")
  }
}
tempVarName <- paste(tempVarName, ")", sep = "")
tuningValue <- eval(parse(text = noquote(tuningValue <- tempVarName)))
set.seed(seedingValue) #ensures reproductability
cl <- makeCluster(2)
registerDoParallel()
multimodel <- caretList(Survived ~ ., data = train, trControl = ctrl, tuneList = tuningValue)
stopCluster(cl)
registerDoSEQ()
closeAllConnections()
#ctrlmulti <- trainControl(number = 2, classProbs = TRUE, summaryFunction = twoClassSummary)

#~~~~~~~~~~ if you have models SEPARATED (due to using the code to create models separately), do this ~~~~~~~~~~~~

tempVarName = paste("list(", methodUsed[1], " = model_list_opt1", sep = "")
for(i in 2:length(methodUsed)){
  tempVarName = paste(tempVarName, ", ", methodUsed[i], " = model_list_opt", i , sep = "")
}
tempVarName = paste(tempVarName, ")", sep = "")
multimodel <- eval(parse(text = tempVarName))
#multimodel <- list(rpart2 = model_list1, xgbTree = model_list2, gbm = model_list3, C5.0 = model_list4, glmboost = model_list5)
class(multimodel) <- "caretList"

if(sum(randomness) > 0) {
  ctrl <- trainControl(method = "repeatedcv", repeats = CVrepeats, number = CVfolds, verboseIter = TRUE, returnResamp = "all", savePredictions = "all", classProbs = TRUE, index = indexPreds, search = "random")
} else {
  ctrl <- trainControl(method = "repeatedcv", repeats = CVrepeats, number = CVfolds, verboseIter = TRUE, returnResamp = "all", savePredictions = "all", classProbs = TRUE, index = indexPreds)
}

#~~~~~~~~~~ END SEPARATION ~~~~~~~~~~

#~~ make ensemble ~~ CHOOSE ###

#cannot use tuneLength
model_corr <- modelCor(resamples(multimodel))
model_pca <- PCA(model_corr, graph = FALSE)
plot.new()
model_plots_vp1 <- viewport(layout.pos.col = 1, layout.pos.row = 1)
model_plots_vp2 <- viewport(layout.pos.col = 2, layout.pos.row = 1)
model_plots_vp3 <- viewport(layout.pos.col = 1, layout.pos.row = 2)
model_plots_vp4 <- viewport(layout.pos.col = 2, layout.pos.row = 2)
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2)))
pushViewport(model_plots_vp1)
par(new = TRUE, fig = gridFIG())
corrplot(model_corr, method = "pie", type = "lower", cl.lim = c(0,1), order = "hclust", tl.col = "black", tl.srt = 45, addCoef.col = "white", bg = "grey95", col = colorRampPalette(c("black", "black", "black", "black", "black", "grey50", "green", "khaki2", "red"))(50))
popViewport()
model_plots <- NULL
model_plots$pca1 <- fviz_contrib(model_pca, choice = "var", axes = 1:2) + theme_linedraw()
model_plots$pca2 <- fviz_pca_var(model_pca, col.var="cos2") + scale_color_gradient2(low="white", mid="blue", high="red", limits = c(0,1)) + theme_minimal()
model_plots$pca3 <- fviz_pca_var(model_pca, col.var="contrib") + scale_color_gradientn(colors = colorRampPalette(c("grey50", "green", "darkgoldenrod", "red"))(50), limits = c(0,100)) + theme_minimal()
pushViewport(model_plots_vp2)
print(model_plots$pca1, newpage = FALSE)
popViewport()
pushViewport(model_plots_vp3)
print(model_plots$pca2, newpage = FALSE)
popViewport()
pushViewport(model_plots_vp4)
print(model_plots$pca3, newpage = FALSE)
popViewport(1)
#grid.arrange(model_plots$pca1, model_plots$pca2, model_plots$pca3, nrow = 2, layout_matrix = cbind(c(1,2),c(1,3)))

multiensemble <- caretStack(multimodel, method = "glm", metric = "Accuracy", trControl = ctrl) #do not use caretEnsemble because it bugs the Summary, GLM bugs a lot due to fixed effects
summary(multiensemble) #summary beforehand
class(multiensemble) <- "caretEnsemble" #return to caretEnsemble
summary(multiensemble) #summary afterhand

#~~ make stack ~~~ CHOOSE ###

multiensemble <- caretStack(multimodel, method = "gbm", tuneLength = ensembleTune, metric = targetUsed, trControl = ctrl)
summary(multiensemble)

#~~ END CHOOSE ###

#if classified ensemble of classifications then returns prob (if prob is set) else returns raw
#if regressed ensemble of classifications then returns raw
#do not pass type value, it does automatically depending on the tree shown higher (depends on ctrlmulti)
predictedValues <- predict(multiensemble, newdata = test)
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
gendermodel$Survived <- as.numeric(predictedValues) - 1
#gendermodel$Survived[predictedValues < 0.50] <- 0
#gendermodel$Survived[predictedValues >= 0.50] <- 1
oldgendermodel <- read.table(oldinputCSV, sep = ",", header = TRUE)
#oldgendermodel <- read.table(newinputCSV, sep = ",", header = TRUE)
print("Difference of predictions: Not different")
print(table(oldgendermodel$Survived == gendermodel$Survived))
write.csv(gendermodel, file = newinputCSV, row.names = FALSE)





#~~~~~ ANALYSIS OF VARIABLES ~~~~


#~~ data set analysis


#~ finds out variable importance (independent/predictors, dependent/predicted), read only one column because reading both is not useful
filterVarImp(data.matrix(train[,-1]),train[,1])

#~ finds linear combinations of numeric variables, requires removing non-numeric variables
findLinearCombos(data.matrix(train)) #trim.matrix...?

#~ near zero varince variables, numeric data only
nearZeroVar(data.matrix(train), names = TRUE)

#~ Recursive Feature Elimination (automatic analysis of predictors)
rfRFE_ctrl <- rfeControl(functions = caretFuncs, method = "repeatedcv", number = 5, repeats = 1, verbose = TRUE, returnResamp = "all")
subsetSize <- c(1:9)
set.seed(seedingValue)
rfProfile <- rfe(train[, -1], train[, 1], sizes = subsetSize, method = "rf", rfeControl = rfRFE_ctrl)
print(rfProfile)
print(rfProfile$fit$finalModel$variable.importance)
#plots
plot(rfProfile, type = c("o", "g")) #normal plot with Accuracy
plot(rfProfile, type = c("o", "g"), metric = "Kappa") #normal plot but with Kappa coefficient
plot(rfProfile, type = c("g", "p", "smooth")) #smoothed plot
densityplot(rfProfile, as.table = TRUE, pch = "|") #density
ggplot(rfProfile) #normal plot
ggplot(data = as.data.frame(rfProfile$variables)[order(rfProfile$variables$Variables),], aes(x = var, y = Overall, fill = as.factor(sort(rfProfile$variables$Variables)))) + geom_bar(stat = "identity") + labs(fill = "Variables")

#~ Selection By Filtering (automatic analysis of predictors)
rfSBF_ctrl <- caretSBF
rfSBF_ctrl$score <- function(x, y) apply(x, 2, caretSBF$score, y = y)
set.seed(seedingValue)
sfProfile <- sbf(train[, -1], train[, 1], method = "rpart2", sbfControl = sbfControl(functions = rfSBF_ctrl, method = "repeatedcv", number = 5, repeats = 1, multivariate = TRUE, verbose = TRUE, returnResamp = "all"))
print(sfProfile)

#~ Selection by Genetic Algorithm (automatic analysis of predictors) | seed 2nd argument = number*repeats (of the CV)
rfGA_ctrl <- gafsControl(functions = caretGA, method = "repeatedcv", number = 2, repeats = 1, seeds = sample.int(seedingValue, 3), verbose = TRUE, returnResamp = "all")
set.seed(seedingValue)
rfProfile <- gafs(x = train[, -1], train[, 1], iters = 2, method = "rpart2", gafsControl = rfGA_ctrl)
print(rfProfile)
plot(rfProfile)

#~ Simulated Annealing (automatic analysis of predictors)
rfSA_ctrl <- safsControl(functions = caretSA, method = "repeatedcv", number = 5, repeats = 1, improve = 10, verbose = TRUE, returnResamp = "all")
set.seed(seedingValue)
saProfile <- safs(x = train[, -1], train[, 1], iters = 300, safsControl = rfSA_ctrl, method = "rpart2")
print(saProfile)
plot(saProfile)


#~~ single analysis of a model


#~ plot a model depending on a tuning method
i <- 2
tempVarName <- paste("model_list", i, sep = "")
plot(get(tempVarName))
histogram(get(tempVarName))
densityplot(get(tempVarName))
xyplot(get(tempVarName))
stripplot(get(tempVarName))

#~ multiplot analysis (lattice)

#method 1
pList <- list(p1 = histogram(get(tempVarName)), p2 = densityplot(get(tempVarName)), p3 = xyplot(get(tempVarName)), p4 = stripplot(get(tempVarName)))
print(pList$p1, split = c(1, 1, 2, 2), more = TRUE)
print(pList$p2, split = c(2, 1, 2, 2), more = TRUE)
print(pList$p3, split = c(1, 2, 2, 2), more = TRUE)
print(pList$p4, split = c(2, 2, 2, 2), more = FALSE)  # more = FALSE is redundant

#method 2
pList <- list(p1 = plot(get(tempVarName), plotType = "scatter", nameInStrip = TRUE), p2 = plot(get(tempVarName), plotType = "level", nameInStrip = TRUE))
print(pList$p1, split = c(1, 1, 2, 1), more = TRUE)
print(pList$p2, split = c(2, 1, 2, 1), more = FALSE)

#~ plot analysis (ggplot2)
ggplot(get(tempVarName), plotType = "scatter", nameInStrip = TRUE, highlight = TRUE) #scatter
ggplot(get(tempVarName), plotType = "level", nameInStrip = TRUE, highlight = TRUE) #level

#~ plot class probabilities
pList <- list(p1 = resampleHist(get(tempVarName), type = "density"), p2 = resampleHist(get(tempVarName), type = "hist"))
print(pList$p1, split = c(1, 1, 1, 2), more = TRUE)
print(pList$p2, split = c(1, 2, 1, 2), more = FALSE)

#~ analyze all models together (metric, sensitivity, specifity)
resamp <- resamples(multimodel) #please coerce train models into variable multimodel
print(resamp$timings) #get compute time required for each model

#~analyze variable importance (1 model)
plot(varImp(get(tempVarName))) #lattice
ggplot(varImp(get(tempVarName))) #ggplot

#another way
predictors(model_list1)

#~analyze variable importance (all models)
tempVarName = paste("t(rbind.fill(as.data.frame(t(varImp(model_list", 1, ",scale = TRUE)$importance[,1, drop = FALSE]))", sep = "")
for(i in 2:length(methodUsed)){
  tempVarName = paste(tempVarName, ", as.data.frame(t(varImp(model_list", i, ",scale = TRUE)$importance[,1, drop = FALSE]))" , sep = "")
}
tempVarName = paste(tempVarName, "))", sep = "")
model_varImp <- eval(parse(text = tempVarName))
model_varImp[is.na(model_varImp)] <- 0
colnames(model_varImp) <- methodUsed
model_varImp <- as.data.frame(add_rownames(as.data.frame(model_varImp), "Variable"))
model_varImp_Sorted <- gather(model_varImp, "Model", "Importance", 2:ncol(model_varImp))

#plot histogram (max = models*100)
ggplot(data = model_varImp_Sorted, aes(x = Variable, y = Importance, fill = Model)) + geom_bar(stat="identity")

#plot vertically facetted histogram (max = 100)
ggplot(data = model_varImp_Sorted, aes(x = Variable, y = Importance, fill = Model, alpha = Importance)) + geom_bar(stat="identity") + facet_grid(Model ~ .) + ylim(0, 100)

#plot horizontally facetted histogram (max = 100)
ggplot(data = model_varImp_Sorted, aes(x = Variable, y = Importance, fill = Model, alpha = Importance)) + geom_bar(stat="identity") + facet_grid(. ~ Model) + ylim(0, 100) + coord_flip()

#plot box plot
ggplot(data = model_varImp_Sorted, aes(x = Variable, y = Importance)) + geom_boxplot()



#~~ ANALYSIS of caretEnsemble ~~

#~ make plot for regressive models
plot(multimodel) #for regressive models only
fortify(multiensemble) #for regressive models only

#~ makes a summary plot for regressive models
#class(multiensemble) <- "caretEnsemble" #if convert to caretEnsemble from caretStack (glm model)
plot(multiensemble)
autoplot(multiensemble, which = c(1:6))

#multiplot analysis (lattice)
pList <- list(p1 = plot(multiensemble$ens_model, plotType = "scatter", nameInStrip = TRUE), p2 = plot(multiensemble$ens_model, plotType = "level", nameInStrip = TRUE))
print(pList$p1, split = c(1, 1, 2, 1), more = TRUE)
print(pList$p2, split = c(2, 1, 2, 1), more = TRUE)



#~~ Analysis of resampling performances - reworked

#create resample list
tempVarName = paste("resamples(list(", methodUsed[1], " = model_list", 1, sep = "")
for(i in 2:length(methodUsed)){
  tempVarName = paste(tempVarName, ", ", methodUsed[i], " = model_list", i , sep = "")
}
tempVarName = paste(tempVarName, "))", sep = "")
resampled_data <- eval(parse(text = tempVarName))

#~ table
summary(resampled_data)

#~ correlation
modelCor(resampled_data)

#~ box and whisker
bwplot(resampled_data, scales = list(x = list(relation = "free"), y = list(relation = "free")))

#~ density
densityplot(resampled_data, scales = list(x = list(relation = "free"), y = list(relation = "free")), pch = "|")

#~ dotplot
dotplot(resampled_data, scales = list(x = list(relation = "free"), y = list(relation = "free")))

#~ parallel
parallelplot(resampled_data)

#~ scatterplot
splom(resampled_data)

#~ pairwise xy plots #specify two model names
xyplot(resampled_data, models = c("cforest", "xgbTree"))

#~ model difference and statistical significance
summary(diff(resampled_data))

#~ prepare ensemble model call
tempVarName = paste("list(", methodUsed[1], " = model_list1", sep = "")
for(i in 2:length(methodUsed)){
  tempVarName = paste(tempVarName, ", ", methodUsed[i], " = model_list", i , sep = "")
}
tempVarName = paste(tempVarName, ")", sep = "")
multimodel <- eval(parse(text = tempVarName))
#multimodel <- list(rpart2 = model_list1, xgbTree = model_list2, gbm = model_list3, C5.0 = model_list4, glmboost = model_list5)
class(multimodel) <- "caretList"
if(sum(randomness) > 0) {
  ctrl <- trainControl(method = "repeatedcv", repeats = CVrepeats, number = CVfolds, verboseIter = TRUE, savePredictions = "final", classProbs = TRUE, summaryFunction = twoClassSummary, search = "random")
} else {
  ctrl <- trainControl(method = "repeatedcv", repeats = CVrepeats, number = CVfolds, verboseIter = TRUE, savePredictions = "final", classProbs = TRUE, summaryFunction = twoClassSummary)
}

#~ make ensemble
multiensemble <- caretStack(multimodel, method = "glm", metric = "ROC", trControl = ctrl) #do not use caretEnsemble because it bugs the Summary, GLM bugs a lot due to fixed effects
summary(multiensemble) #summary beforehand
class(multiensemble) <- "caretEnsemble" #return to caretEnsemble
summary(multiensemble) #summary afterhand

class(multiensemble) <- "caretStack" #return to caretStack if needed by the user

#~ export to CSV
predictedValues <- predict(multiensemble, newdata = test) #some strange things can happen, like binary inversion
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
gendermodel$Survived <- as.numeric(predictedValues) - 1
#gendermodel$Survived[predictedValues < 0.50] <- 0
#gendermodel$Survived[predictedValues >= 0.50] <- 1
oldgendermodel <- read.table(oldinputCSV, sep = ",", header = TRUE)
#oldgendermodel <- read.table(newinputCSV, sep = ",", header = TRUE)
print("Difference of predictions: Not different - False = changed, True = not changed")
print(table(oldgendermodel$Survived == gendermodel$Survived))

#check for strange things happening (binary inversion)
if(as.data.frame(table(oldgendermodel$Survived == gendermodel$Survived))[1,2] > as.data.frame(table(oldgendermodel$Survived == gendermodel$Survived))[2,2]) {
  gendermodel$Survived <- gendermodel$Survived + 1
  gendermodel$Survived[gendermodel$Survived == 2] <- 0
  print("Swapped True/False")
  print(table(oldgendermodel$Survived == gendermodel$Survived))
} else {
  print("Not swapped True/False")
}
write.csv(gendermodel, file = newinputCSV, row.names = FALSE)

#~ plot ensemble - use caretEnsemble to see all models
plot(multiensemble)

#~ variable importance, only for GLM/LM
varImp(multiensemble)

#~ variable importance, only between models
varImp(multiensemble$ens_model)

#~ estimated AUC/ROC on whole dataset
model_preds <- lapply(multimodel, predict, newdata = train)
model_preds <- data.frame(model_preds)
model_preds$ensemble <- predict(multiensemble, newdata = train)
model_preds <- as.data.frame(lapply(model_preds, as.numeric)) - 1
caTools::colAUC(model_preds, train$Survived, plotROC = TRUE, alg = "ROC")

#~ variable average contribution
coef(multiensemble$ens_model$finalModel)[-1]/sum(coef(multiensemble$ens_model$finalModel)[-1])