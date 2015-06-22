library(ggplot2)
library(kernlab)

testdata <- generateTwoCircles(1000,noise=0.1)
testdata[-sample(1:nrow(testdata),10),]$Class <- NA
tvec<-na.omit(testdata$Class)
dmatU <- as.matrix(testdata[is.na(testdata$Class),1:2])
dmat <- as.matrix(testdata[!is.na(testdata$Class),1:2])

plotframe <- data.frame(dmatU, Output=GRFClassifier(dmat, tvec, dmatU, adjacency_kernel = rbfdot(10))@unlabels)

ggplot(plotframe,aes(x=X.1,y=X.2,color=y1)) +
  geom_point()
