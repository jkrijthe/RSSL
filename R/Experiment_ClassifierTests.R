library(mlbench)

# Initialize dataset
modelform<-formula("factor(classes)~.")
classname<-all.vars(modelform)[1]
distance<-1
D<-data.frame(mlbench.2dnormals(500,2,distance))
D[51:500,classname]<-rep(NA,450)
D_test<-data.frame(mlbench.2dnormals(10000,2,distance))

res<-SSLDataFrameToMatrices(modelform,D,intercept=TRUE)
X<-res$X
X_u<-res$X_u
y<-res$y

res<-SSLDataFrameToMatrices(modelform,D_test,intercept=TRUE)
X_test<-res$X
y_test<-res$y

## LeastSquaresClassifier
g<-LeastSquaresClassifierXY(X,y)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

g<-LeastSquaresClassifier(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

#Test scaling
g<-LeastSquaresClassifierXY(X,y,scale=TRUE)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

#Test Regularization
g<-LeastSquaresClassifierXY(X,y,lambda=0.1)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

## ICLeastSquaresClassifier
g<-ICLeastSquaresClassifierXY(X,y,X_u)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

g<-ICLeastSquaresClassifier(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

#Test scaling
g<-ICLeastSquaresClassifierXY(X,y,X_u,scale=TRUE)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

#Test Regularization
g<-ICLeastSquaresClassifierXY(X,y,X_u,lambda1=0.2,lambda2=0.2)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

## NearestMeanClassifier
g<-NearestMeanClassifierXY(X[,2:ncol(X)],y)
print(1-mean(predict(g,X_test[,2:ncol(X)])==y_test))
loss(g, X_test[,2:ncol(X)], y_test)

g<-NearestMeanClassifier(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

#Test scaling
g<-NearestMeanClassifierXY(X[,2:ncol(X)],y,scale=TRUE)
print(1-mean(predict(g,X_test[,2:ncol(X)])==y_test))
loss(g, X_test[,2:ncol(X)], y_test)

## MCNearestMeanClassifier
g<-MCNearestMeanClassifierXY(X[,2:ncol(X)],y, X_u[,2:ncol(X)])
print(1-mean(predict(g, X_test[,2:ncol(X)])==y_test))
loss(g, X_test[,2:ncol(X)], y_test)

g<-MCNearestMeanClassifier(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

#Test scaling
g<-MCNearestMeanClassifierXY(X[,2:ncol(X)],y, X_u[,2:ncol(X)],scale=TRUE)
print(1-mean(predict(g, X_test[,2:ncol(X)])==y_test))
loss(g, X_test[,2:ncol(X)], y_test)

## SelfLearning
g<-SelfLearningXY(X,y,X_u,method=LeastSquaresClassifierXY)
print(1-mean(predict(g, X_test)==y_test))
loss(g, X_test, y_test)

## LDA classifier
g<-LinearDiscriminantClassifierXY(X[,2:ncol(X)],y)
print(1-mean(predict(g,X_test[,2:ncol(X)])==y_test))
loss(g, X_test[,2:ncol(X)], y_test)

g<-LinearDiscriminantClassifier(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

## Moment Constrained LDA Classifier
g<-MCLinearDiscriminantClassifierXY(X[,2:ncol(X)],y, X_u[,2:ncol(X)])
print(1-mean(predict(g, X_test[,2:ncol(X)])==y_test))
loss(g, X_test[,2:ncol(X)], y_test)

g<-MCLinearDiscriminantClassifier(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

## QDA classifier
g<-QuadraticDiscriminantClassifierXY(X[,2:ncol(X)],y)
print(1-mean(predict(g,X_test[,2:ncol(X)])==y_test))
loss(g, X_test[,2:ncol(X)], y_test)

g<-QuadraticDiscriminantClassifier(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

## Logistic Regression
g<-LogisticRegressionXY(X,y)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

g<-LogisticRegression(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

#Test scaling
g<-LogisticRegressionXY(X,y,scale=TRUE)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

#Test Regularization
g<-LogisticRegressionXY(X,y,lambda=3)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

## Entropy Regularized Logistic Regression
g<-EntropyRegularizedLogisticRegressionXY(X,y,X_u)
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)

g<-EntropyRegularizedLogisticRegression(modelform, D)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

## Transductive SVM



# library(mlbench)
# 
# # Settings
# repeats<-100
# distance<-2
# 
# # Initialization
# modelform<-formula("factor(classes)~.")
# classname<-all.vars(modelform)[1]
# results<-matrix(nrow=repeats,ncol=8)
# 
# D_pop<-data.frame(mlbench.hypercube(10000,1))
# D_test<-data.frame(mlbench.hypercube(10000,1))
# 
# #D_pop<-data.frame(mlbench.2dnormals(10000,cl=2))
# #D_test<-data.frame(mlbench.2dnormals(10000,cl=2))
# 
# # Create Dataset 
# i_train<-strata(D_pop,classname,rep(8,2),method="srswor")$ID_unit
# D_train<-D_pop[i_train,]
# i_l <- createDataPartition(D_train[,classname], p = .5, list = FALSE, times = 1)
# D_l <- D_train[i_l,]
# D_u <- D_train[-i_l,]
# 
# D_u[,classname]<-rep(NA,nrow(D_u))
# D_train<-rbind(D_l,D_u)
# 
# p<-qplot(x,classes,data=D_l)
# g<-FisherClassifier(modelform,D_l)
# p<-p+geom_abline(intercept=g@theta[1],slope=g@theta[2],color="red")
# g<-SemiSupervisedFisherClassifier(modelform,D_train)
# p<-p+geom_abline(intercept=g@theta[1],slope=g@theta[2],color="blue")
# p

#s<-FisherClassifier(modelform,D_l)
#p+geom_abline(intercept = (1.5-s@theta[1])/s@theta[3], slope = -s@theta[2]/s@theta[3])


# #Test Allabelings
# alllabelings(c(0,1),10)
# 
# g_ga<-gaconstrainedlr(modelform, D_train)
# 
# #Test logLik function
# #TODO: still doesn't work for small samples when the data is separable
# #TODO: still doesn't work if there is just one class
# library(sampling)
# D_pop<-data.frame(mlbench.2dnormals(10000,2,distance))
# for (j in 1:100) {
#   i<-strata(D_pop,classname,c(2,2),method="srswor")$ID_unit
#   D_l<-D_pop[i,]
#   
#   modelform<-formula("factor(classes)~.")
#   classname<-all.vars(modelform)[1]
#   m<-glm(modelform, data=D_l, family=binomial("logit"))
#   
#   
#   y<-as.numeric(data.matrix(D_l[,classname]))-1
#   X<-data.matrix(cbind(rep(1,nrow(D_l)),D_l[,!(colnames(D_l) %in% c(classname)), drop=FALSE]))
#   
#   if (abs( loglikelihood_logisticregression(m$coefficients,X,y)-logLik(m) )>1.0e-4) {
#     print(loglikelihood_logisticregression(m$coefficients,X,y))
#     print(logLik(m))
#   }
# }