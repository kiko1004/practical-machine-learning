data("iris"); library("ggplot2")
library(caret)
g <- qplot(Petal.Length, Sepal.Width, data = iris, color = Species)
g + geom_smooth(method = "lm")
modelfit <- train(Species ~ ., data = iris, method = "rpart")
plot(modelfit$finalModel)
text(modelfit$finalModel, use.n = T, all = T, cex = 0.8)
library(rattle)
fancyRpartPlot(modelfit$finalModel)
prediction <- predict(modelfit, newdata = iris)
a <- confusionMatrix(prediction, iris$Species)
a$table

RFfit <- train(Species~., data = iris, method = "rf", prox = T)
RFfit
pred <- predict(RFfit, newdata = iris)
confusionMatrix(pred, iris$Species)
iris$testingTrue <- pred == iris$Species
table(pred, iris$Species)
qplot(Petal.Width, Sepal.Length, data = iris, color = testingTrue)



modlda <- train(Species~., data = iris, method = "lda")
modnb <- train(Species~., data = iris, method = "nb")
