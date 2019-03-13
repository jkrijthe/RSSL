#' USMLeastSquaresClassifier
#' @include LeastSquaresClassifier.R
setClass("USMLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY",y_scale="numeric"), 
         prototype(name="USMLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

#' Updated Second Moment Least Squares Classifier
#' 
#' This methods uses the closed form solution of the supervised least squares problem, except that the second moment matrix (X'X) is exchanged with a second moment matrix that is estimated based on all data. See for instance \cite{Shaffer1991}, where in this implementation we use all data to estimate E(X'X), instead of just the labeled data. This method seems to work best when the data is first centered \code{x_center=TRUE} and the outputs are scaled using \code{y_scale=TRUE}.
#' 
#' @family RSSL classifiers
#' 
#' @references Shaffer, J.P., 1991. The Gauss-Markov Theorem and Random Regressors. The American Statistician, 45(4), pp.269-273.
#' 
#' @param lambda numeric; L2 regularization parameter
#' @inheritParams BaseClassifier
#' 
#' @export
USMLeastSquaresClassifier<-function(X, y, X_u, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, y_scale=FALSE, ...,use_Xu_for_scaling=TRUE) {
	  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center,use_Xu_for_scaling=use_Xu_for_scaling)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  #y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  y <- ModelVariables$Y[,1,drop=FALSE]
  if (y_scale) {
    y_scale <- mean(y)
    y <- y-y_scale
  } else {
    y_scale <- 0
  }
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
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
  Y <- ModelVariables$Y[,1,drop=FALSE]
  
  if (is.null(Y)) { stop("No labels supplied.")}
  if (ncol(Y)!=1) { stop("Multiclass loss not implemented yet")}
  return((X %*% object@theta + object@y_scale - Y)^2)
})

#' @rdname rssl-predict
#' @aliases predict,USMLeastSquaresClassifier-method
setMethod("predict", signature(object="USMLeastSquaresClassifier"), function(object, newdata,...) {
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
  return(classes)
  
})