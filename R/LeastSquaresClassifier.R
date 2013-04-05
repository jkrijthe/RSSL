# Formal class definition
setClass("LeastSquaresClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY"),
         prototype(name="LeastSquaresClassifier",scaling=NULL), 
         contains="Classifier")

LeastSquaresClassifierXY <- function(X, y, lambda=0, scale=FALSE, ...) {
  
  if (scale) {
    library(pls)
    scaling<-stdize(X[,2:ncol(X),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(X)]<-predict(scaling,X[,2:ncol(X),drop=FALSE])
    
  } else {scaling=NULL}
  
  n<-nrow(X)
  m<-ncol(X)
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% y)
  
  new("LeastSquaresClassifier",
      classnames=1:length(unique(y)),
      scaling=scaling,
      theta=theta)
}

# Constructor
LeastSquaresClassifier <- function(modelform,D,lambda=0,scale=FALSE) {
  list2env(SSLDataFrameToMatrices(modelform,D,intercept=TRUE),environment())
  
  trained<-LeastSquaresClassifierXY(X, y, lambda, scale)
  trained@modelform<-modelform
  trained@classnames<-classnames
  return(trained)
}

setMethod("loss", signature(object="LeastSquaresClassifier"), function(object, newdata, y=NULL) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    if (is.null(y)) { stop("No labels supplied.")}
    X<-newdata
  }
  #Scale the data
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  
  
  return(sum((X %*% object@theta - y)^2))
})

setMethod("predict", signature(object="LeastSquaresClassifier"), function(object, newdata, probs=FALSE) {
  
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    X<-X
  } else {
    if (!is.matrix(newdata)) { browser(); stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  
  
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore>1.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  
  if (probs)
  {
    return(expscore)
  } else return(classes)
})



setMethod("plot", signature(x="LeastSquaresClassifier",y="missing"), function(x) {
  object<-x
  #p<-qplot(object@D[is.na(object@D[,object@classname]),1],object@D[is.na(object@D[,object@classname]),2],color=object@unlabels)
  p<-qplot(object@D[,1],object@D[,2],color=object@D[,object@classname])
  p<-p+geom_abline(intercept = (1.5-x@theta[1])/x@theta[3], slope = -x@theta[2]/x@theta[3])
  return(p)
})