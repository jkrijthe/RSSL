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
  X <- ModelVariables$X
  Y <- ModelVariables$Y
  y <- ModelVariables$y
  
  if (is.null(prior)) prior <- matrix(colMeans(Y),2,1)
  means <- t((t(X) %*% Y))/(colSums(Y))
  sigma <- mean((X-(Y %*% means))^2)
  sigma <- diag(ncol(X))*sigma
  sigma <- lapply(1:ncol(Y),function(c){sigma})
  
  ## Return object
  new("NearestMeanClassifier", 
      modelform=ModelVariables$modelform, 
      prior=prior, 
      means=means, 
      sigma=sigma,
      classnames=ModelVariables$classnames,
      scaling=ModelVariables$scaling)
}
