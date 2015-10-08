library(ggplot2)
library(kernlab)

testdata <- generateTwoCircles(1000,noise=0.05)
testdata[-sample(1:nrow(testdata),6),]$Class <- NA
tvec<-na.omit(testdata$Class)
dmatU <- as.matrix(testdata[is.na(testdata$Class),1:2])
dmat <- as.matrix(testdata[!is.na(testdata$Class),1:2])

precision<-50
xgrid<-seq(min(dmatU[,1]),max(dmatU[,1]),length.out=precision)
ygrid<-seq(min(dmatU[,2]),max(dmatU[,2]),length.out=precision)
gridmat <- expand.grid(xgrid,ygrid)

g_sup <- KernelLeastSquaresClassifier(
            dmat, tvec, 
            kernel=rbfdot(1), lambda=0.001, scale = TRUE)

g_kernel<- LaplacianKernelLeastSquaresClassifier(
              X=dmat, y=tvec, X_u=dmatU, 
              kernel=rbfdot(1), lambda=0.001, gamma=500, 
              adjacency_kernel = rbfdot(1), scale = TRUE)

plotframe <- cbind(gridmat, decisionvalues(g_sup,gridmat))
colnames(plotframe)<- c("x","y","Output")
ggplot(plotframe, aes(x=x,y=y)) +
  geom_tile(aes(fill = Output)) +
  stat_contour(aes(z=Output),breaks=c(0.5),size=1) +
  scale_fill_gradient(low="yellow", high="red",limits=c(0,1)) +
  geom_point(aes(x=X1,y=X2,shape=Class,size=Class),data=testdata) +
  scale_shape_discrete(na.value=8) +
  scale_size_discrete(range=c(5,5),na.value=0.5)

plotframe <- cbind(gridmat, decisionvalues(g_kernel,gridmat))
colnames(plotframe)<- c("x","y","Output")
ggplot(plotframe, aes(x=x,y=y)) +
  geom_tile(aes(fill = Output)) +
  stat_contour(aes(z=Output),breaks=c(0.5),size=1) +
  scale_fill_gradient(low="yellow", high="red",limits=c(0,1)) +
  geom_point(aes(x=X1,y=X2,shape=Class,size=Class),data=testdata) +
  scale_shape_discrete(na.value=8) +
  scale_size_discrete(range=c(5,5),na.value=0.5)

testdata <- generateCrescentMoon(100,sigma=0.3)
testdata[-sample(1:nrow(testdata),10),]$Class <- NA
tvec<-na.omit(testdata$Class)
dmatU <- as.matrix(testdata[is.na(testdata$Class),2:3])
dmat <- as.matrix(testdata[!is.na(testdata$Class),2:3])

precision<-50
xgrid<-seq(min(dmatU[,1]),max(dmatU[,1]),length.out=precision)
ygrid<-seq(min(dmatU[,2]),max(dmatU[,2]),length.out=precision)
gridmat <- expand.grid(xgrid,ygrid)

g_sup <- KernelLeastSquaresClassifier(
              dmat, tvec, 
              kernel=rbfdot(0.25), lambda=0.001, scale = TRUE)

g_kernel <- LaplacianKernelLeastSquaresClassifier(
              X=dmat, y=tvec, X_u=dmatU, 
              kernel=rbfdot(0.25), lambda=0.001, gamma=500, 
              adjacency_kernel = rbfdot(10), scale = TRUE)

g_kernel <- KernelICLeastSquaresClassifier(
              X=dmat, y=tvec, X_u=dmatU, 
              kernel=rbfdot(0.25), lambda=0.001, scale = TRUE, 
              classprior=0.5,lambda_prior=100, projection = "supervised")

plotframe <- cbind(gridmat, decisionvalues(g_sup,gridmat))
colnames(plotframe)<- c("x","y","Output")
p_sup <- ggplot(plotframe, aes(x=x,y=y)) +
  geom_tile(aes(fill = Output)) +
  stat_contour(aes(z=Output),breaks=c(0.5),size=1) +
  scale_fill_gradient(low="yellow", high="red",limits=c(0,1)) +
  geom_point(aes(x=X1,y=X2,shape=Class,size=Class),data=testdata) +
  scale_shape_discrete(na.value=8) +
  scale_size_discrete(range=c(5,5),na.value=0.5)

plotframe <- cbind(gridmat, decisionvalues(g_kernel,gridmat))
colnames(plotframe)<- c("x","y","Output")
p_kern <- ggplot(plotframe, aes(x=x,y=y)) +
  geom_tile(aes(fill = Output)) +
  stat_contour(aes(z=Output),breaks=c(0.5),size=1) +
  scale_fill_gradient(low="yellow", high="red",limits=c(0,1)) +
  geom_point(aes(x=X1,y=X2,shape=Class,size=Class),data=testdata) +
  scale_shape_discrete(na.value=8) +
  scale_size_discrete(range=c(5,5),na.value=0.5)

df_imputed <- data.frame(dmatU,y=g_kernel@responsibilities+g_kernel@y_scale)
p_unlabels <- ggplot(df_imputed,aes(x=X1,y=X2,color=y)) +geom_point()

library("gridExtra")
grid.arrange(p_sup,p_kern,p_unlabels,ncol=2)
