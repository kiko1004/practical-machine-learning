library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

training_IL <- training[c("diagnosis", names(training)[grep("^IL",colnames(training))])]
testing_IL <- testing[c("diagnosis", names(testing)[grep("^IL",colnames(testing))])]

## PCA
preProc <- preProcess(training_IL[, -1], method=c("center", "scale", "pca"), thresh=0.8) ## 9 columns
trainPC <- predict(preProc, training_IL) ## 10 columns
testPC <- predict(preProc, testing_IL)   ## 10 columns
modelFit <- train(diagnosis ~ ., method="glm", data=trainPC)
confusionMatrix(testing_IL$diagnosis, predict(modelFit, testPC)) ## Accuracy : 0.7195


## Non-PCA
modelFit2 <- train(diagnosis ~ ., method="glm", data=training_IL)
confusionMatrix(testing_IL$diagnosis, predict(modelFit2, testing_IL)) ## Accuracy : 0.6463
