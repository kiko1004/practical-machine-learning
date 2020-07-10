library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


library(Hmisc)
cuts8 <- cut2(training$CompressiveStrength, g =4)
library(ggplot2)
windows()
p1 <- qplot(cuts8, Age, data= training, fill = cuts8, geom = "boxplot")
p2 <- qplot(cuts8, FineAggregate, data= training, fill = cuts8, geom = "boxplot")
p3 <- qplot(cuts8, Water, data= training, fill = cuts8, geom = "boxplot")
p4 <- qplot(cuts8, Cement, data= training, fill = cuts8, geom = "boxplot")
multiplot(p1, p2, p3, p4, cols=2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


library(Hmisc)
training$CompressiveStrength <- cut2(training$CompressiveStrength, seq(0, 100, by = 10))
featurePlot(x=training, y=training$CompressiveStrength, plot="pairs")

