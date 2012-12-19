# Formal class definition
setClass("LogisticRegression", representation(theta="matrix"), prototype(name="LogisticRegression"), contains="Classifier")

# Constructor Method
LogisticRegression<-function(modelform,D) {
  classname<-all.vars(modelform)[1]
  
  y<-as.numeric(data.matrix(D[,classname]))-1
  X<-data.matrix(cbind(rep(1,nrow(D)),D[,!(colnames(D) %in% c(classname)), drop=FALSE]))
  
  theta <- c(0.0,0.0,0.0)
  object <- new("LogisticRegression", modelform=modelform,D_l=D_l,D_u=D_u, theta=theta)
  opt_result<-optim(theta,logLik.LogisticRegression,gr=NULL,X,y,control=list(fnscale=-1))
  theta<-opt_result$par
  return(object)
}

setMethod("logLik", signature(object="LogisticRegression", D="data.frame"), function(object, D) {
  theta<-object@theta
  X<-
  y<-
  
  sum(y*(X %*% theta) - log(1+exp(X%*%theta)))
}

setMethod("logLik", signature(object="LogisticRegression", X="matrix"), function(object, X,y) { {
  sum(y*(X %*% objects@theta) - log(1+exp(X %*% object@theta)))
}
                                                                                                
library(sampling)
library(caret)

distance<-0.5
D_test<-data.frame(mlbench.2dnormals(1000,2,distance))
D_pop<-data.frame(mlbench.2dnormals(10000,2,distance))
#D_test<-generateBananaSet(10000,2,2)
#D_pop<-generateBananaSet(10000,2,2)

i_train<-strata(D_pop,classname,c(10,10),method="srswor")$ID_unit
D_train<-D_pop[i_train,]
i_l <- createDataPartition(D_train[,classname], p = .5, list = FALSE, times = 1)
D_l <- D_train[i_l,]
D_u <- D_train[-i_l,]
D_u[,classname]<-rep(NA,nrow(D_u))
D_train<-rbind(D_l,D_u)


logLik(lr(modelform,D_l))
print(LogisticRegression(modelform,D_l))
ImplicitlyConstrainedLogisticRegression(modelform,D_train)

D_train<-Class.Gaussian(20,3)
i_l <- createDataPartition(D_train[,class], p = .5, list = FALSE, times = 1)
D_l <- D_train[i_l,]
D_u <- D_train[-i_l,]
D_u[,classname]<-rep(NA,nrow(D_u))
D_train<-rbind(D_l,D_u)

g_impconstrained <- icnm(modelform, D_train)
g_impconstrained$parameters.allowed



Class.Gaussian<-function(n,distance) {
  library(mvtnorm)
  data.frame(X1=rbind(rmvnorm(n,-distance/2),rmvnorm(n,distance/2)), class=c(rep(0,n),rep(1,n)))
}

Class.TwoMoons <- function() {}
Class.Checkerboard <- function() {}
