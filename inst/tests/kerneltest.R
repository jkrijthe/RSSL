library(kernlab)

precision<-200

dmat<-model.matrix(Species~.-1,iris[51:150,])
tvec<-droplevels(iris$Species[51:150])

data<-generateCrescentMoon(150,1)
dmat<-model.matrix(Class~.-1,data)
tvec<-data$Class

xgrid<-seq(min(dmat[,1]),max(dmat[,1]),length.out=precision)
ygrid<-seq(min(dmat[,2]),max(dmat[,2]),length.out=precision)
gridmat<-as.matrix(expand.grid(xgrid,ygrid))

g_kernel<-LeastSquaresClassifier(dmat[,1:2],tvec,kernel=rbfdot(20),intercept=FALSE,lambda=0,scale = TRUE)
g_linear<-LeastSquaresClassifier(dmat[,1:2],tvec,intercept=TRUE)
z<-matrix(predict(g_kernel,gridmat,probs=TRUE),precision,precision)

# z<-matrix(as.integer(predict(SVM(dmat[,1:2],tvec,kernel=polydot(degree=10),intercept=FALSE,lambda=1e10),gridmat)),precision,precision)

image(xgrid,ygrid,z)
points(dmat[,1],dmat[,2],pch=as.integer(factor(tvec)))
#contour(xgrid,ygrid,z)


library(rgl)
plot3d(x = dmat[,1],y=dmat[,2],z=predict(g_trained,dmat[,1:2]))
