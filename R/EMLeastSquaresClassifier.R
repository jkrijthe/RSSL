#' @include LinearDiscriminantClassifier.R
setClass("EMLeastSquaresClassifier",
         representation(responsibilities="ANY"),
         prototype(name="Expectation Maximization Least Squares Classifier"),
         contains="LeastSquaresClassifier")

#' An Expectation Maximization like approach to Semi-Supervised Least Squares Classification
#' 
#' Minimize the total loss of the labeled and unlabeled objects by finding the weight vector and labels that minimize the total loss. The algorithm proceeds similar to EM, by subsequently applying a weight update and a soft labeling of the unlabeled objects. This is repeated until convergence.
#' 
#' @param X design matrix of the labeled objects
#' @param y vector with labels
#' @param X_u design matrix of the labeled objects
#' @param method character; Currently only "EM"
#' @param scale Should the features be normalized? (default: FALSE)
#' @param eps Stopping criterion for the maximinimization
#' @param verbose logical; Controls the verbosity of the output
#' @param ... Additional Parameters, Not used
#' 
#' @export
EMLeastSquaresClassifier <- function(X, y, X_u, method="inverse", x_center=FALSE, scale=FALSE, verbose=FALSE, intercept=TRUE,lambda=0,eps=10e-10) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  Y<-ModelVariables$Y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- Y

  
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(Y)
  
  Xe<-rbind(X,X_u)
  
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  resp <- rep(0.5,nrow(X_u))
  resp_old <- rep(Inf,nrow(X_u))
  
  iterations <- 0
  while (mean(abs(resp-resp_old))>eps) {
    Ye <- c(Y,resp)
    if (method=="inverse") {
      if (intercept) {
        theta <- inv(t(Xe) %*% Xe + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(Xe) %*% t(t(Ye)))
      } else {
        theta <- inv(t(Xe) %*% Xe + n*lambda*diag(rep(1,m))) %*% (t(X) %*% t(t(Ye)))
      }
    }
    resp_old <- resp
    resp <- X_u %*% theta
    resp <- sapply(resp, function(x) {max(c(0,min(c(x,1))))})
    iterations <- iterations + 1
    if (verbose) print(sum(resp-resp_old))
  }
  
  if (verbose) { cat("Number of iterations: ",iterations,"\n")}
  new("EMLeastSquaresClassifier", 
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      responsibilities=as.numeric(resp))
}
