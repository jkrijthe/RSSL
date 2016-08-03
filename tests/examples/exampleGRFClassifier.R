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
                        Output=classifier@responsibilities[,1])

ggplot(plotframe,aes(x=X1,y=X2,color=Output)) +
  geom_point() + coord_equal()

data <- generateParallelPlanes()
data$Class <- NA
data$Class[1] <- "a"
data$Class[101] <- "b"
data$Class[201] <- "c"
data$Class <- factor(data$Class)

GRFClassifier(Class~.,data,NULL)@responsibilities

data <- generateSpirals()
data$Class<-NA
data$Class[1] <- "a"
data$Class[200] <- "b"
data$Class <- factor(data$Class)

GRFClassifier(Class~.,data,NULL)@responsibilities
