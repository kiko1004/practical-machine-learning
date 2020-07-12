#Q1----------------------
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
inTrain <- createDataPartition(segmentationOriginal$Case, p = 0.75, list = F)
train <- segmentationOriginal[inTrain,]
test <- segmentationOriginal[-inTrain,]
set.seed(125)
model_1 <- caret::train(Class ~. , data = train, method = "rpart")
windows()
library(rattle)
fancyRpartPlot(model_1$finalModel)
predict(model_1, newdata = data.frame(TotalIntench2 = 23000, FiberWidthCh1 =10, PerimStatusCh1=2))

        
#Q2-------------
library(pgmm)
data(olive)
olive = olive[,-1]
model_2 <- caret::train(Area ~. , data = olive, method = "rpart")
predict(model_2, newdata = as.data.frame(t(colMeans(olive))))


#Q3---------------
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

#Q4 ---------------
