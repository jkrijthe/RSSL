#' @include NormalBasedClassifier.R
setClass("QuadraticDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Quadratic Discriminant Classifier"),
         contains="NormalBasedClassifier")

#' Quadratic Discriminant Classifier
#'
#' Implementation of the quadratic discriminant classifier. Classes are modeled as Gaussians with different covariance matrices. The optimal covariance matrix and means for the classes are found using maximum likelihood, which, in this case, has a closed form solution.
#'
#' @family RSSL classifiers
#'
#' @param prior A matrix with class prior probabilities. If NULL, this will be estimated from the data
#' @inheritParams BaseClassifier
#' @return S4 object of class LeastSquaresClassifier with the following slots:
#' \item{modelform}{weight vector}
#' \item{prior}{the prior probabilities of the classes}
#' \item{mean}{the estimates means of the classes}
#' \item{sigma}{The estimated covariance matrix}
#' \item{classnames}{a vector with the classnames for each of the classes}
#' \item{scaling}{scaling object used to transform new observations}
#' @export
QuadraticDiscriminantClassifier <- function(X, y, prior=NULL, scale=FALSE,  ...) {
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  
  #Set priors if not set by user
  if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
  
  #Calculate means for classes
  means <- t((t(X) %*% Y))/(colSums(Y))
  
  #Set sigma to be the average within scatter matrix
  sigma <- lapply(1:ncol(Y),function(c,X){cov(X[Y[,c]==1,,drop=FALSE])},X)

    
  new("QuadraticDiscriminantClassifier", modelform=modelform, prior=prior, means=means, sigma=sigma,classnames=classnames,scaling=scaling)
}

#' @rdname line_coefficients-methods
#' @aliases line_coefficients,QuadraticDiscriminantClassifier-method 
setMethod("line_coefficients", signature(object="QuadraticDiscriminantClassifier"), function(object) {
  stop("Not a linear decision boundary.")
})
