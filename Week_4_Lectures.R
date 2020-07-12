data("iris")
KMeans <- kmeans(iris[,-5], centers = 3)
iris$clusters <- as.factor(KMeans$cluster)
qplot(Petal.Width, Sepal.Length, data = iris, colour = clusters)
