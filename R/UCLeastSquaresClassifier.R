#' UCLeastSquaresClassifier
#' @include LeastSquaresClassifier.R
setClass("UCLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY"), 
         prototype(name="UCLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

#' Updated Covariance Least Squares Classifier
#' 
#' See for instance \cite{Shaffer1991}. Use all data to estimate E(X'X). TODO: check whether this works?
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
#' 
UCLeastSquaresClassifier<-function(X, y, X_u, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, y_scale=FALSE, ...) {
	  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  #y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  if (length(classnames)>2) {
    y <- model.matrix(~y-1, data.frame(y))
  } else {
    y <- model.matrix(~y-1, data.frame(y))[,1,drop=FALSE]
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
  new("UCLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept
      )
}
