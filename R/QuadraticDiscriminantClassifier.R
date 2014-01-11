#' @include NormalBasedClassifier.R
setClass("QuadraticDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Quadratic Discriminant Classifier"),
         contains="NormalBasedClassifier")

#' Quadratic Discriminant Classifier
#'
#' Implementation of the quadratic discriminant classifier. Classes are modeled as gaussians with different covariance matrices. The optimal covariance matrix and means for the classes are found using maximum likelihood, which, in this case, has a closed form solution.
#'
#' @usage QuadraticDiscriminantClassifier(X, y, method="closedform",prior=NULL, scale=FALSE, ...)
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param method the method to use. Either "closedform" for the fast closed form solution or "ml" for explicit maximum likelihood maximization
#' @param prior A matrix with class prior probabilites. If NULL, this will be estimated from the data
#' @param scale If TRUE, apply a z-transform to the design matrix X before running the regression
#' @param ... additional arguments
#' @return S4 object of class LeastSquaresClassifier with the following slots:
#' \item{modelform}{weight vector}
#' \item{prior}{the prior probabilities of the classes}
#' \item{mean}{the estimates means of the classes}
#' \item{sigma}{The estimated covariance matrix}
#' \item{classnames}{a vector with the classnames for each of the classes}
#' \item{scaling}{scaling object used to transform new observations}
#' @export
QuadraticDiscriminantClassifier <- function(X, y, method="closedform", prior=NULL, scale=FALSE,  ...) {
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
    #Set priors if not set by user
    if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
    
    #Calculate means for classes
    means<-t((t(X) %*% Y))/(colSums(Y))
    
    #Set sigma to be the average within scatter matrix
    sigma<-lapply(1:ncol(Y),function(c,X){cov(X[Y[,c]==1,,drop=FALSE])},X)
    
  } else if (method=="ml") {
    stop("QDC: ML optimization not implemented yet.")
    opt_func<-function(theta, X, y) {
      means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
      sigma<-matrix(theta[((ncol(Y)*ncol(X))+1):length(theta)],ncol(X),ncol(X))
      
      
      sigma<-lapply(1:ncol(Y),function(c){sigma})
      
      model<-new("QuadraticDiscriminantClassifier", prior=prior, means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
      
      loss(model,X,y)
    }
    
    theta<-rep(0.01,ncol(Y)*ncol(X)+2 * (ncol(X)^2))
    opt_result <- optim(theta, opt_func, gr=NULL, X, y, method="L-BFGS-B", lower=c(-Inf,-Inf,0.000000001),control=list(trace=1,maxit=1000))
    theta<-opt_result$par
    
    means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
    sigma<-theta[(ncol(Y)*ncol(X))+1]
    sigma<-diag(ncol(X))*sigma
    sigma<-lapply(1:ncol(Y),function(c){sigma})
  }
  new("QuadraticDiscriminantClassifier", modelform=modelform, prior=prior, means=means, sigma=sigma,classnames=classnames,scaling=scaling)
}
