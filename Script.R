#install.packages("caret")
#install.packages("kernlab")
#install.packages("ISLR")
library(caret)
library(kernlab)
library(ISLR)
data("spam")
inTrain <- createDataPartition(spam$type, p = 0.75, list = F)
taining <- spam[inTrain,]
testing <- spam[-inTrain,]
modell <- train(type ~., data = taining, method = "glm")
summary(modell)
modell
prediction <- predict(modell, taining)
confusionMatrix(prediction, testing$type)
preObj <- preProcess(taining[,-58], method = c("center","scale"))
capaveTr <- predict(preObj, testing[,-58])$capitalAve


data("Wage")
summary(Wage)
inTrain <- createDataPartition(y= Wage$wage, p = 0.7, list = F)
training <- Wage[inTrain,]
testing <- Wage [-inTrain,]
windows()
featurePlot(x=training[,c("age", "education", "jobclass")]
            , y = training$wage, plot = "pairs")
library(ggplot2)
library(Hmisc)
cutwage <- cut2(training$wage, g =3)
p1 <- qplot(cutwage, age, data= training, fill = cutwage, geom = "boxplot")
p1
