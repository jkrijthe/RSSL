library(ggplot2)
library(kernlab)

testdata <- generateTwoCircles(200,noise=0.1)
testdata[-sample(1:nrow(testdata),10),]$Class <- NA
tvec<-na.omit(testdata$Class)
dmatU <- as.matrix(testdata[is.na(testdata$Class),1:2])
dmat <- as.matrix(testdata[!is.na(testdata$Class),1:2])

classifier <- GRFClassifier(dmat, tvec, dmatU, 
                adjacency_kernel = rbfdot(10))

plotframe <- data.frame(dmatU, 
                        Output=classifier@responsibilities)

ggplot(plotframe,aes(x=X1,y=X2,color=Output)) +
  geom_point() + coord_equal()
