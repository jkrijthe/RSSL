#' @include NormalBasedClassifier.R
setClass("NearestMeanClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Nearest Mean Classifier"),
         contains="NormalBasedClassifier")

#' Nearest Mean Classifier
#'
#' Implementation of the nearest mean classifier modeled as a gaussian for each class. Classes are modeled as gaussians with equal, spherical covariance matrices. The optimal covariance matrix and means for the classes are found using maximum likelihood, which, in this case, has a closed form solution. To get true nearest mean classification, set prior as a matrix with equal probabilty for all classes.
#'
#' @param prior A matrix with class prior probabilites. If NULL, this will be estimated from the data
#' @param method the method to use. Either "closedform" for the fast closed form solution or "ml" for explicit maximum likelihood maximization
#' @inheritParams BaseClassifier
#' 
#' @return S4 object of class LeastSquaresClassifier with the following slots:
#' \item{modelform}{weight vector}
#' \item{prior}{the prior probabilities of the classes}
#' \item{mean}{the estimates means of the classes}
#' \item{sigma}{The estimated covariance matrix}
#' \item{classnames}{a vector with the classnames for each of the classes}
#' \item{scaling}{scaling object used to transform new observations}
#' @export
NearestMeanClassifier <- function(X, y, method="closedform", prior=NULL, x_center=FALSE, scale=FALSE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=NULL,
                                x_center=x_center, 
                                scale=scale,
                                intercept=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
    if (is.null(prior)) prior <- matrix(colMeans(Y),2,1)
    means <- t((t(X) %*% Y))/(colSums(Y))
    sigma <- mean((X-(Y %*% means))^2)
    sigma <- diag(ncol(X))*sigma
    sigma <- lapply(1:ncol(Y),function(c){sigma})
    
  } else if (method=="ml") {
    
    if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
    
    opt_func<-function(theta, X, y) {
      means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
      sigma<-theta[(ncol(Y)*ncol(X))+1]
      sigma<-diag(ncol(X))*sigma
      sigma<-lapply(1:ncol(Y),function(c){sigma})
      
      model<-new("NearestMeanClassifier", 
                 modelform=modelform, 
                 prior=prior, 
                 means=means, 
                 sigma=sigma,
                 classnames=1:ncol(Y),
                 scaling=scaling)
      loss(model,X,y)
    }
    
    theta<-rep(0.01,3)
    opt_result <- optim(theta, opt_func, gr=NULL, X, y, method="L-BFGS-B", lower=c(-Inf,-Inf,0.000000001))
    theta<-opt_result$par
    
    means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
    sigma<-theta[(ncol(Y)*ncol(X))+1]
    sigma<-diag(ncol(X))*sigma
    sigma<-lapply(1:ncol(Y),function(c){sigma})
  }
  
  ## Return object
  new("NearestMeanClassifier", 
      modelform=modelform, 
      prior=prior, 
      means=means, 
      sigma=sigma,
      classnames=classnames,
      scaling=scaling)
}
