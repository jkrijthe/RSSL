setGeneric("loss",
           function(object, ...) standardGeneric("loss")
)

# Formal class definition
setClass("LDAClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",threshold="ANY"),
         prototype(name="LDAClassifier",scaling=NULL), 
         contains="Classifier")

# Constructor
LDAClassifier<-function(modelform,D,lambda=0,method="direct") {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  X <- model.matrix(modelform, D)
  
  y <- as.factor(data.matrix(D_l[,classname]))
  classnames <- levels(y)
  if (length(classnames)>2){
    y<-(model.matrix(~0+.,data=D[,classname,drop=FALSE]))
  } else {
    y<-as.integer(y)
  }
  
  X<-X[,2:ncol(X),drop=FALSE]
  
  m<-ncol(X)
  
  
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  if (method=="direct") {
    means <- array(unlist(by(X,y,colMeans)),c(m,1,length(classnames)))
    Sclass <- array(unlist(by(X,y,cov)),c(m,m,length(classnames)))
    p<-tabulate(y)/length(y)
    Sw <- p[1]*Sclass[,,1]+p[2]*Sclass[,,2]
  }
  if (method="ML") {
    opt_func <- function(theta) {
      p<-
      means<-
      Sw<-
      
      
      return(sum((X %*% theta - y)^2)) 
    }

    opt_grad <- function(theta) {
      # Add derivative
    }
    
    opt_result <- optim()
  }
  
  
  
  theta <- inv(Sw) %*% (means[,,1,drop=FALSE]-means[,,2,drop=FALSE])
  threshold <- t(theta) %*% (means[,,1,drop=FALSE]+means[,,2,drop=FALSE])
  
  new("LDAClassifier",
      modelform=modelform,
      classname=classname,
      classnames=classnames,
      D=D,
      theta=theta,
      threshold=as.numeric(threshold))
}



setMethod("loss", signature(object="LDAClassifier"), function(object, D) {
  X<-model.matrix(object@modelform, D)
  y<-data.matrix(as.integer(D[,object@classname]))
  return(sum((X %*% object@theta - y)^2))
})

setMethod("predict", signature(object="LDAClassifier"), function(object, D, probs=FALSE) {
  
  D[,object@classname] <- 1
  X<-model.matrix(modelform,D)
  X<-X[,2:ncol(X),drop=FALSE]
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  browser()
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore<object@threshold)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  if (probs)
  {
    return(expscore)
  } else return(classes)
})