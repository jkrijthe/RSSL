#' LogisticLossClassifier
#' @include LeastSquaresClassifier.R
setClass("CPLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY"), 
         prototype(name="CPLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

#' @title Contrastive Pressimistic Least Squares Classifier
#'
#' @description Loog's Contrastive Pressimism applied to the Least Squares Classifier
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix of the unlabeled data, intercept term is added within the function
#' @param lambda1 Regularization parameter in the unlabeled+labeled data regularized least squares
#' @param lambda2 Regularization parameter in the labeled data only regularized least squares
#' @param intercept TRUE if an intercept should be added to the model
#' @param x_center logical; Whether the feature vectors should be centered
#' @param scale logical; If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param method Either "default" for solving using L-BFGS-B gradient descent or "QP" fpr a quadratic programming based solution
#' @param max_iterations integer; Number of iterations in minimax solving
#' @param alpha numeric; Learning rate in minimax solving
#' @param eps numeric; Convergence criretion in minimax solving
#' @param ... additional arguments
#' @return S4 object of class CPLeastSquaresClassifier with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' @export
CPLeastSquaresClassifier<-function(X, y, X_u=NULL, lambda1=0, lambda2=0, intercept=TRUE,x_center=FALSE,scale=FALSE,method="default",max_iterations=10000, alpha=1000, eps=1e-8, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { ginv(M) } #Another possibility: chol2inv(chol(M))
  
  if (nrow(X_u)==0) Xe<-X
  else Xe <- rbind(X,X_u)
  
  
  m <- ncol(Xe)
  n_u <- nrow(X_u)
  
  
  objective <- function(g,w,X,y,X_u) {
    mean((Xe %*% w - matrix(c(y,g),ncol=1))^2)-mean((Xe %*% w_sup - matrix(c(y,g),ncol=1))^2)
  }

  gradient <- function(g,w,X,y,X_u) {
    (1/(nrow(Xe)))*((X_u %*% w - matrix(1,nrow(X_u),1))^2 - (X_u %*% w - matrix(2,nrow(X_u),1))^2 - (X_u %*% w_sup - matrix(1,nrow(X_u),1))^2 + (X_u %*% w_sup - matrix(2,nrow(X_u),1))^2)
  }
  
  G <- (inv(t(Xe) %*% Xe)) %*% t(Xe)

  g <- rep(1.5,n_u)
  w_sup <- (inv(t(X) %*% X)) %*% t(X) %*% y
  w <- G %*% matrix(c(y,g+1),ncol=1)

  objective_value_old <- objective(g,w,X,y,X_u)
  #   print(objective_value_old)
  for (i in 1:max_iterations) {
    stepsize<-alpha*(1/max_iterations)
    # For given labeling, find optimal parameters
    w <- G %*% c(y,g)
    print(objective(g,w,X,y,X_u))
    # For a given parameter vector make a step
    g <- g - alpha*gradient(g,w,X,y,X_u)
    
    # Simplified projection
    g <- sapply(g,function(x) { max(min(2,x),1)})
    
    # Check if we should stop
    objective_value <- objective(g,w,X,y,X_u)
    #print(objective_value)
    if (abs(objective_value_old-objective_value)<eps) {
      break
    } else {
      objective_value_old<-objective_value
    }
  }
  print(i)
  
  
  new("CPLeastSquaresClassifier",
      classnames=classnames,
      modelform=modelform,
      theta=w,
      unlabels=g,
      scaling=scaling,
      intercept=intercept
  )
}
