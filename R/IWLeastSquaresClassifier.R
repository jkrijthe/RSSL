#' IWLeastSquaresClassifier
#' @include LeastSquaresClassifier.R
setClass("IWLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY"), 
         prototype(name="IWLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

IWLeastSquaresClassifier<-function(X,y,X_u,lambda=0, intercept=TRUE, scale=FALSE, y_scale=FALSE, method="uniform", ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
  X<-ModelVariables$X
  y<-ModelVariables$y
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")

	
if (y_scale) {
    y_scaling<-mean(y)
    y<-y-y_scaling
  } else {
    y_scaling<-NULL
  }

  #There is a problem using ginv when using PCs as inputs: the problems seem to be rescaled such that the bias term is no longer correct
  
  ## Start Implementation
  n<-nrow(X)
  m<-ncol(X)
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  Xe<-rbind(X,X_u)

  if (method=="clustering") {
  	# Determine weights based on a clustering
  	clustering<-kmeans(Xe,centers=100,nstart=1000)
    #TODO redo this section
  	cluster_probs<-prop.table(table(clustering$cluster))
  	assignments<-NULL #TODO: get cluster assignments
  	importanceweights<-cluster_probs[assignments]
  	importanceweights<-importanceweights/sum(importanceweights)
  } else if (method=="kernel") {
  	# Determine weights using a kernel density estimator
  } else {
  	importanceweights<-rep(1/n,n)
  }
  W<-diag(importanceweights)

  # Use a kernel density estimator to weight the labeled points in the regression

  if (intercept) {
    theta <- inv((n/nrow(X)) * t(X) %*% W %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% W %*% y)
  } else {
    theta <- inv((n/nrow(X)) * t(X) %*% W %*% X + n*lambda*diag(rep(1,m))) %*% (t(X) %*% W %*% y)
  }
  
  ## Return correct object
  new("IWLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      y_scaling=y_scaling
      )

}
