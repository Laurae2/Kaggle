#~~~~~~~~ new algorithm

#~~~~ load packages

library(caret)
library(caretEnsemble)
library(Amelia)
library(stringr)
library(plyr)
library(doParallel) #do not parallelize because it creates major issues

#~~~~ clean up

remove(list = ls())

#~~~~~~~~~~~~~~ USER INPUT ~~~~~~~~~~~~~~

#~~~~ what methods?
set.seed(11111) #ensures reproductability
#methodUsed <- c("rpart2", "xgbTree", "gbm", "C5.0", "glmboost")
methodUsed <- c("C5.0", "glmboost")
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

#~~~~~ for each model, what's the specs?

#~~ how many tunings per model? (mutliplicated by CV folds x repeats x number of parameters of the specific model)
tune <- c(0)
#tune[1] <- 25
#tune[2] <- 5
#tune[3] <- 10
#tune[4] <- 5
#tune[5] <- 10
print(cbind(methodUsed, tune))

specs = c("0")
for(i in 1:length(methodUsed)){
  specs[i] <- paste(methodUsed[i], " = caretModelSpec(method = \"", methodUsed[i], "\", tuneLength = ", tune[i], ")", sep = "")
}

randomness = c(0)
for(i in 1:length(methodUsed)){
  randomness[i] <- TRUE
}
randomness[3] <- FALSE #disables randomness for C5.0 because it crashes


#~~~~~ for each index, what columns? (single tuning)
#col = c(0) #deprecated, not working
for(i in 1:length(methodUsed)){
  tempVarName1 = paste("col", i, sep = "")
  assign(tempVarName1, c("Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked"))
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
CVrepeats = 5
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


#~~ fixing missing data
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
inTrainingSet <- createDataPartition(train$Survived, p = .8, list = FALSE)

#~~ or LOAD DATA
test <- train[-inTrainingSet,]
train <- train[ inTrainingSet,]

#~~ launching model

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
    ctrl <- trainControl(method = "adaptive_cv", repeats = CVrepeats, number = CVfolds, savePredictions = TRUE, classProbs = TRUE, index = createMultiFolds(train$Survived, CVfolds, CVrepeats), search = "random")
  } else {
    ctrl <- trainControl(method = "adaptive_cv", repeats = CVrepeats, number = CVfolds, savePredictions = TRUE, classProbs = TRUE, index = createMultiFolds(train$Survived, CVfolds, CVrepeats))
  }
  set.seed(11111) #ensures reproductability
  assign(tempVarName1, train(Survived ~ ., data = train_temp, method = methodUsed[i], trControl = ctrl, tuneLength = tune[i]))
  stopCluster(cl)
  registerDoSEQ()
  closeAllConnections()
}

#confusion matrix
for(i in 1:length(methodUsed)){
  print(paste("Doing ", methodUsed[i], "(", i, ")", sep = ""))
  tempVarName <- paste("model_list", i, sep = "")
  tempVarName2 <- paste("col", i, sep = "")
  tempVarName3 <- "bin"
  test_temp <- test[, c("Survived", get(tempVarName2))]
  if (!is.na(get(tempVarName3)[i])[1]){
    for(Vars in get(tempVarName3)[i]){
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
tempVarName <- paste("model_list", i, sep = "")
model_list <- get(tempVarName)
#stopCluster(cl)
predictedValues <- predict.train(model_list, test_temp, type = "raw")
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
#gendermodel$Survived[predictedValues == "No"] <- 0
#gendermodel$Survived[predictedValues == "Yes"] <- 1
oldgendermodel <- read.table(oldinputCSV, sep = ",", header = TRUE)
print("Difference of predictions: Not different")
print(count(oldgendermodel$Survived == gendermodel$Survived))
write.csv(gendermodel, file = newinputCSV, row.names = FALSE)




tuning <- list(
  rf1=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
  rf2=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=10), preProcess="pca"),
  nn=caretModelSpec(method="nnet", tuneLength=2, trace=FALSE)
)
tuning



#~~~~ another predictive usage possibility using caretEnsemble, more accurate very quickly

if (length(randomness[randomness == 0]) == 0) {
  ctrl <- trainControl(method = "adaptive_cv", repeats = CVrepeats, number = CVfolds, savePredictions = "final", classProbs = TRUE, index = createMultiFolds(train$Survived, CVfolds, CVrepeats), search = "random")
} else {
  ctrl <- trainControl(method = "adaptive_cv", repeats = CVrepeats, number = CVfolds, savePredictions = "final", classProbs = TRUE, index = createMultiFolds(train$Survived, CVfolds, CVrepeats))
}
tempVarName = paste("list(", specs[1], sep = "")
if (length(methodUsed) != 1) {
  for(i in 2:length(methodUsed)) {
    tempVarName <- paste(tempVarName, ", ", specs[i], sep = "")
  }
}
tempVarName <- paste(tempVarName, ")", sep = "")
tuningValue <- eval(parse(text = noquote(tuningValue <- tempVarName)))
set.seed(11111) #ensures reproductability
multimodel <- caretList(Survived ~ ., data = train, trControl = ctrl, methodList = methodUsed, tuneList = tuningValue)
#ctrlmulti <- trainControl(number = 2, classProbs = TRUE, summaryFunction = twoClassSummary)
multiensemble <- caretEnsemble(multimodel)
summary(multiensemble)
predictedValues <- predict(multiensemble, newdata = test, type = "raw")
gendermodel <- read.table("gendermodel.csv", sep = ",", header = TRUE)
#gendermodel$Survived[predictedValues < 0.50] <- 0
#gendermodel$Survived[predictedValues >= 0.50] <- 1
oldgendermodel <- read.table(oldinputCSV, sep = ",", header = TRUE)
print("Difference of predictions: Not different")
print(count(oldgendermodel$Survived == gendermodel$Survived))
write.csv(gendermodel, file = newinputCSV, row.names = FALSE)
