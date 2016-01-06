context("Laplacian SVM")

library(ggplot2)
library("gridExtra")

testdata <- generateSlicedCookie(expected=TRUE)
testdata$Class[sample(1:nrow(testdata),nrow(testdata)-10)] <- NA
t_svm <- SVM(Class~.,testdata, C=1,kernel=rbfdot(0.25),scale=FALSE)
t_lapsvm <- LaplacianSVM(Class~., testdata, kernel=rbfdot(0.25), lambda=0.05, gamma=100,scale=FALSE)
t_lapsvm@bias
t_svm@bias

# library("createdatasets")

testdata <- generateCrescentMoon(100,sigma=0.3)
testdata[-sample(1:nrow(testdata),10),]$Class <- NA
tvec<-na.omit(testdata$Class)
dmatU <- as.matrix(testdata[is.na(testdata$Class),2:3])
dmat <- as.matrix(testdata[!is.na(testdata$Class),2:3])

precision <- 50
xgrid<-seq(min(dmatU[,1]),max(dmatU[,1]),length.out=precision)
ygrid<-seq(min(dmatU[,2]),max(dmatU[,2]),length.out=precision)
gridmat <- expand.grid(xgrid,ygrid)

t_svm <- SVM(dmat, tvec, kernel=rbfdot(0.1), C=1, scale = FALSE)
t_lapsvm <- LaplacianSVM(X=dmat, y=tvec, X_u=dmatU, kernel=rbfdot(0.1), lambda=0.05, gamma=100, adjacency_kernel = rbfdot(0.1), scale = FALSE)

plotframe <- cbind(gridmat, decisionvalues(t_svm,gridmat))
colnames(plotframe)<- c("x","y","Output")
p_svm <- ggplot(plotframe, aes(x=x,y=y)) +
  geom_tile(aes(fill = Output)) +
  stat_contour(aes(z=Output),breaks=c(-1,0.0,1),size=1) +
  scale_fill_gradient(low="yellow", high="red",limits=c(-1,1)) +
  geom_point(aes(x=X1,y=X2,shape=Class,size=Class),data=testdata) +
  scale_shape_discrete(na.value=8) +
  scale_size_discrete(range=c(5,5),na.value=0.5)

plotframe <- cbind(gridmat, decisionvalues(t_lapsvm,gridmat))
colnames(plotframe)<- c("x","y","Output")
p_lapsvm <- ggplot(plotframe, aes(x=x,y=y)) +
  geom_tile(aes(fill = Output)) +
  stat_contour(aes(z=Output),breaks=c(-1,0.0,1),size=1) +
  scale_fill_gradient(low="yellow", high="red",limits=c(-1,1)) +
  geom_point(aes(x=X1,y=X2,shape=Class,size=Class),data=testdata) +
  scale_shape_discrete(na.value=8) +
  scale_size_discrete(range=c(5,5),na.value=0.5)

#grid.arrange(p_svm, p_lapsvm, ncol=2)

test_that("LaplacianSVM is the same as SVM when manifold term has weight 0", FALSE)