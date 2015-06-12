#' USMLeastSquaresClassifier
#' @include LeastSquaresClassifier.R
setClass("USMLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY",y_scale="numeric"), 
         prototype(name="USMLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

#' Updated Second Moment Least Squares Classifier
#' 
#' See for instance \cite{Shaffer1991}. Use all data to estimate E(X'X).
#' 
#' @param X Design matrix
#' @param y labels
#' @param X_u Design matrix unlabeled data
#' @param lambda numeric; L2 regularization parameter
#' @param intercept logical; Whether an intercept should be added to the model
#' @param scale logical; Whether the features should be scaled
#' @param y_scale logical; Whether the class vector should be scaled
#' @param x_center logical; Whether the features should be centered
#' @param ... Unused
#' 
#' @export
USMLeastSquaresClassifier<-function(X, y, X_u, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, y_scale=FALSE, ...) {
	  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  #y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  y <- ModelVariables$Y
  if (y_scale) {
    y_scale <- mean(y)
    y <- y-y_scale
  } else {
    y_scale <- 0
  }
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")

  # There is a problem using ginv when using PCs as inputs: the problems seem to be rescaled such that the bias term is no longer correct
  
  ## Start Implementation
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(y)
  
  Xtrain<-NULL
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  Xe<-rbind(X,X_u)

  if (intercept) {
    theta <- inv((n/nrow(Xe)) * t(Xe) %*% Xe + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% y)
  } else {
    theta <- inv((n/nrow(Xe))* t(Xe) %*% Xe + n*lambda*diag(rep(1,m))) %*% (t(X) %*% y)
  }
  
  ## Return correct object
  new("USMLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      y_scale=y_scale
      )
}

#' @rdname loss-methods
#' @aliases loss,USMLeastSquaresClassifier-method
#' @export
setMethod("loss", signature(object="USMLeastSquaresClassifier"), function(object, newdata, y=NULL,...) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  Y <- ModelVariables$Y
  
  if (is.null(Y)) { stop("No labels supplied.")}
  if (ncol(Y)!=1) { stop("Multiclass loss not implemented yet")}
  return((X %*% object@theta + object@y_scale - Y)^2)
})

#' @rdname rssl-predict
#' @aliases predict,USMLeastSquaresClassifier-method
setMethod("predict", signature(object="USMLeastSquaresClassifier"), function(object, newdata, probs=FALSE,...) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta + object@y_scale
  
  # If we need to return classes
  if (ncol(theta)>1) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore[,1]<0.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  
  if (probs){
    return(expscore)
  } else {
    return(classes)
  }
})