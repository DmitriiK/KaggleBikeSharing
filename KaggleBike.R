# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(randomForest)
library(Metrics)
library(foreach)
library(gbm)
library(party)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/Dimas/Documents/R/KaggleBikeSharing/")

load("RawData.rda")
load("TestData.rda")

# Further partitioning our original training data into training and test sets
inTrain         = createDataPartition(RawData$count, p = 0.8)[[1]]
training        <- RawData[inTrain,]
testing         <- RawData[-inTrain,]


# Now it's time to build various models for predicting casual and registered separately. 
# Their sum is our responce, count, and we'll carpture more variance and will 
# be able to make better predictions if we predict them separately and then add together. 
# 
# First the test and training sets were used to build models and evaluate their performance.
# Then same models were trained on all the provided labeled data in order to increase accuracy. 
# The code has models trained on all the data already, and therefore contains only optimistic 
# estimates of RMSLE. During the model selection training and testing sets were used as well 
# for "real" estimate.
# 
# #############################################

# 
# Build a Random Forest model and save it. Then estimate optimistic RMSLE.
# RF does not require cross validation to properly test OOB performance.

# RF for casual
RFfit <- randomForest(RawData$casual~.,  
                      data = RawData[-(10:11)], 
                      mtry = 3, 
                      ntree = 4000)
save(RFfit, file = "RFfit.rda")

load("RFfit.rda")
prediction <- round(predict(RFfit, newdata = RawData[-(10:11)]))
save(prediction, file = "prediction.rda")
load("prediction.rda")
# Optimistic estimate of RMSLE for RF for casual = 0.419. Not very good.
rmsle(RawData$casual, prediction)

# RF for registered 
RFfit1 <- randomForest(RawData$registered~.,  
                       data = RawData[-c(9,11)], 
                       mtry = 3, 
                       ntree = 4000)
save(RFfit1, file = "RFfit1.rda")

load("RFfit1.rda")
prediction12 <- predict(RFfit1, newdata = RawData[-c(9,11)])
save(prediction12, file = "prediction12.rda")
load("prediction12.rda")
# Optimistic estimate of RMSLE for RF for registered = 0.444. Not very good at all.
rmsle(RawData$registered, prediction12)

# RMSLE estimate

# Optimistic estimate of RMSLE for RF for sum of registered and casual = 0.435. 
# Not very promising, but worth a try.
pr<- (prediction  + prediction12)
rmsle(RawData$count, pr)



########################################################
########################################################
########################################################



#GLM poisson

# Parameters for caret's train
fitControl <- trainControl(method = "repeatedcv",        # do repeated Cross Validation
                           number = 10,                  # 10-fold
                           repeats = 4)                  # repeat 4 times each 
# GLM for registered
Lfit <- train(
              registered~.,  
              data = RawData[-c(9,11)],
              method="glm",
              trControl = fitControl,
              family=poisson)
save(Lfit, file = "Lfit.rda")
load("Lfit.rda")

# GLM for casual
Lfit2 <- train(
        casual~.,  
        data = RawData[-(10:11)],
        method="glm",
        trControl = fitControl,
        family=poisson)
save(Lfit2, file = "Lfit2.rda")
load("Lfit2.rda")

# RMSLE optimistic estimate:

# Making predictions
pred <- round(predict(Lfit, newdata = RawData))
pred2 <- round(predict(Lfit2, newdata = RawData))

# Optimistic estimate of RMSLE with glm = 0.596. Random forest was better.
rmsle(RawData$count, (pred+pred2))



########################################################
########################################################
########################################################




# Conditional inference trees

# Parameters for caret's train
fitControl <- trainControl(method = "repeatedcv",        # do repeated Cross Validation
                           number = 10,                  # 10-fold
                           repeats = 4)                  # repeat 4 times each 
# Ctree for casual
ctree <- train(casual~.,  
                data = RawData[-(10:11)],
               method = "ctree",
               tuneLength = 5,
               trControl = fitControl
                )
save(ctree, file = "ctree.rda")

# and for registered
ctree1 <- train(registered~., 
                data = RawData[-c(9,11)],
                method = "ctree",
                tuneLength = 5,
                trControl = fitControl
                )
save(ctree1, file = "ctree1.rda")

# Predictions
predCT <- round(predict(ctree, newdata = RawData))
predCT2 <- round(predict(ctree1, newdata = RawData))
save(predCT, file = "predCT.rda")
save(predCT2, file = "predCT2.rda")

# RMSLE optimistic estimate is 0.345. 
# This is the best so far and it performed the best on kaggle test set.
rmsle(RawData$count, (predCT+predCT2))





########################################################
########################################################
########################################################



# GBM model: terribe performance. First is was trained with caret in order to get optimal number of trees, 
# shrinkage and interaction depth. Even with the optimal parameters performance is bad. 

# GBM for casual
GBMfit <- gbm(casual~.,  
              data = RawData[-(10:11)],
              n.trees = 3000,
              interaction.depth = 2,
              verbose = FALSE,
              shrinkage = 0.005,
              cv.folds = 5,                     #5-fold cross validation
              distribution = "poisson")
save(GBMfit, file = "GBMfit.rda")
load("GBMfit.rda")

# and for registered
GBMfit2 <- gbm(registered~., 
               data = RawData[-c(9,11)],
              n.trees = 3000,
              interaction.depth = 2,
              shrinkage = 0.005,
              cv.folds = 5,                     #5-fold cross validation
              verbose = FALSE,
              distribution = "poisson")
save(GBMfit2, file = "GBMfit2.rda")
load("GBMfit2.rda")

# Predictions
prediction <- round(predict(GBMfit, n.trees = 3000, newdata = RawData))
prediction1 <- round(predict(GBMfit2, n.trees = 3000, newdata = RawData))

# RMSLE optimistic estimate is 2.75, which is terrible.
rmsle(RawData$count, (prediction+prediction1))



########################################################
########################################################
########################################################



# Making final predictions on kaggle test set and making a submission csv.
# Predictions from all the models were submited to kaggleone by one, and 
# conditional inference tree turned out to be the best with RMSLE 0.50962.
p1 <- predict(ctree, newdata = TestData)
p2 <- predict(ctree1, newdata = TestData)

TestData$count = round(p1 + p2)
save(file="TestData.rda", x=TestData)

Submission <- read.csv(file = "test.csv")
Submission <- Submission[1]
Submission$count <- TestData$count
write.csv(Submission, "SubmissionCT.csv", row.names=FALSE)
