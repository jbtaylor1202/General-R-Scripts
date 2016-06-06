#https://xgboost.readthedocs.io/en/latest//R-package/xgboostPresentation.html


#########
#IMPORTS#
#########
library(xgboost)

#Import train and test data
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

#Examine datasets
str(train)
dim(train$data)
dim(test$data)
class(train$data)[1]
class(train$label)


##########
#TRAINING#
##########
#Basic Training
bstSparse <- xgboost(data = train$data,
                     label = train$label,
                     max.depth = 2, #the trees won't be deep, because our case is very simple
                     eta = 1,
                     nthread = 2,#the number of cpu threads we are going to use
                     nround = 2, #there will be two passes on the data, the second one will enhance the model by further reducing the difference between ground truth and prediction.
                     objective = "binary:logistic",
                     verbose = 2) #train a binary classification model

#Adding verbose = 0 supresses outputs
#Adding verbose = 1 prints evaluation metric
#Adding verbose = 2 prints evaluation metric and tree info


############
#PREDICTION#
############
#Basic Prediction
pred <- predict(bstSparse, test$data)
#Transform predictions to classifications
prediction <- as.numeric(pred > 0.5)


############
#EVALUATION#
############
err <- mean(as.numeric(pred > 0.5) != test$label) #Average Error
print(paste("test-error=", err))


##########
#ADVANCED#
##########
#Need to convert data to xgb.DMatrix
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)

#Measure learning progress with xgb.train
watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain,
                 max.depth=2,
                 eta=1,
                 nthread = 2,
                 nround=6,
                 watchlist=watchlist,
                 objective = "binary:logistic")

#Adding different evaluation metrics
bst <- xgb.train(data=dtrain,
                 max.depth=2,
                 eta=1,
                 nthread = 2,
                 nround=2,
                 watchlist=watchlist,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 objective = "binary:logistic")

#Linear boosting (add booster and remove eta)
#Sometimes a better choice than the previous boosted trees approach particaulalry if linear link between predictor and outcome
bst <- xgb.train(data=dtrain,
                 booster = "gblinear",
                 max.depth=2,
                 nthread = 2,
                 nround=2,
                 watchlist=watchlist,
                 eval.metric = "error",
                 eval.metric = "logloss",
                 objective = "binary:logistic")

######################
#SAVE AND LOAD MODELS#
######################
xgb.DMatrix.save(dtrain, "dtrain.buffer")
dtrain2 <- xgb.DMatrix("dtrain.buffer")
bst <- xgb.train(data=dtrain2, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, objective = "binary:logistic")

##############
#EXTRACT INFO#
##############
#Label data
label <- getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

#View feature importance/influence from the learnt model¶
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#View the trees from a model
xgb.dump(bst, with.stats = T)
xgb.plot.tree(model = bst)






