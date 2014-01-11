#' LogisticLossClassifier
#' @include LeastSquaresClassifier.R
setClass("ICLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY"), 
         prototype(name="ICLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

#' Implicitly Constained Least Squares Classifier
#'
#' Implicitly constrained semisupervised learning with least squares loss. Least squares regression is used treating classes as targets (1 for one class, 2 for the other). Implemented using matrix inversions, not the more stable Singular Value Decomposition method. We find an (fractional) labelling of the unlabeled objects which, whose least squares regression solution minimizes the least squares loss on the labeled training data only. This is equivalent to finding a weighting of the unlabeled objects belonging to either of the two classes.
#'
#' @usage ICLeastSquaresClassifier(X, y, X_u=NULL, lambda1=0, lambda2=0, intercept=TRUE,scale=FALSE,method="default", ...)
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix of the unlabeled data, intercept term is added within the function
#' @param lambda1 Regularization parameter in the unlabeled+labeled data regularized least squares
#' @param lambda2 Regularization parameter in the labeled data only regularized least squares
#' @param intercept TRUE if an intercept should be added to the model
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param method Either "default" for solving using L-BFGS-B gradient descent or "QP" fpr a quadratic programming based solution
#' @param ... additional arguments
#' @return S4 object of class ICLeastSquaresClassifier with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' @export
ICLeastSquaresClassifier<-function(X, y, X_u=NULL, lambda1=0, lambda2=0, intercept=TRUE,x_center=TRUE,scale=FALSE,method="default",lambda3=0,trueprob=NULL,...) {
  
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
  
  if (!is.null(trueprob)) {
    mean_y<-trueprob
  } else {
    mean_y<-mean(y)
  }
  
  m<-ncol(Xe)

  C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1))))) 
  F<- X %*% C 
  G <- X_u %*% t(F) %*% F
  

  ## Quadratic Programming implementation
  if (method=="QP") {
    dvec <- X_u %*% C %*% t(X) %*% y
    Dmat <- G %*% t(X_u)
    Amat <- t(rbind(diag(nrow(X_u)), -diag(nrow(X_u))))
    
    # prior constraint
    
    
    bvec <- c(rep(1,nrow(X_u)), rep(-2,nrow(X_u)))
    #browser()
    #LowRankQP(Dmat,dvec,Amat,bvec,uvec,method="PFCF",verbose=FALSE,niter=200)@alpha
    #ipop(dvec, Dmat, l=0, u=0)
    theta<-solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)$solution
    
    #browser()
    #theta<-ipop(-dvec, Dmat, A=rbind(diag(nrow(X_u)),rep(1/nrow(X_u),nrow(X_u))), b=c(rep(1,nrow(X_u)),mean(y)), r=c(rep(1,nrow(X_u)),mean(y)), l=rep(1,nrow(X_u)), u=rep(2,nrow(X_u)))@primal
    unlabels<-theta
    opt_result<-0
    
  }
  # TODO: change this!
  if (method=="euclidean") {

    w_sup<-LeastSquaresClassifier(X,y)@w # Supervised coefficients

    opt_func <- function(theta) {
      # Return a Mahanalobis type distance between the semi-supervised and supervised coefficients

      theta<-matrix(theta)
      w_semi <- C %*% t(Xe) %*% rbind(matrix(y),theta)
      if (lambda2>0) { 
        error("Not implemented") #todo
      }
      else { 
        return((w_semi-w_sup) %*% t(X) %*% X %*%  (w_semi-w_sup))   
      }
    }
    
    O1 <- 2/nrow(X) * G %*% t(X) %*% y
    O2 <- 2/nrow(X) * G %*% t(X_u)
    O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
    
    if (lambda2>0) {
      O4<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% t(F) %*% y
      O5<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% C %*% t(X_u)
    }
    
    opt_grad <- function(theta) {
      theta<-matrix(theta)
      reg3<-lambda3*2*(rep(sum(theta),nrow(theta))-rep(nrow(theta)*mean_y,nrow(theta)))

      if (lambda2>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta + reg3)
      else return(O1 + O2 %*% theta - O3 + reg3  ) 
    }
    
    theta <- rep(1.5,nrow(X_u))
    # Bounded optimization
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1))
    theta<-opt_result$par
    
    unlabels<-theta
  } else if (method=="projection") {
    
    
    opt_func <- function(theta) {
      # Return a Mahanalobis type distance between the semi-supervised and supervised coefficients
      
      theta<-matrix(theta)
      w_semi <- C %*% t(Xe) %*% rbind(matrix(y),theta)
      if (lambda2>0) { 
        error("Not implemented") #todo
      }
      else { 
        return((w_semi-w_sup) %*% t(X) %*% X %*%  (w_semi-w_sup))   
      }
    }
    
    O1 <- 2/nrow(X) * G %*% t(X) %*% y
    O2 <- 2/nrow(X) * G %*% t(X_u)
    O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
    
    if (lambda2>0) {
      O4<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% t(F) %*% y
      O5<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% C %*% t(X_u)
    }
    
    opt_grad <- function(theta) {
      theta<-matrix(theta)
      reg3<-lambda3*2*(rep(sum(theta),nrow(theta))-rep(nrow(theta)*mean_y,nrow(theta)))
      
      if (lambda2>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta + reg3)
      else return(O1 + O2 %*% theta - O3 + reg3  ) 
    }
    
    theta <- rep(1.5,nrow(X_u))
    # Bounded optimization
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1))
    theta<-opt_result$par
    
    unlabels<-theta
  }
  ## Gradient Descent implementation
  else {
    opt_func <- function(theta) {
      theta<-matrix(theta)
      theta <- C %*% t(Xe) %*% rbind(matrix(y),theta)
      if (lambda2>0) { 
        if (intercept) { return(mean((X %*% theta - y)^2) + lambda2 * sum(theta[2:nrow(theta),1]^2))}
        else  { return(mean((X %*% theta - y)^2) + lambda2 * sum(theta^2)) + lambda3*(mean(theta)-mean_y)^2 }
      }
      else { return(mean((X %*% theta - y)^2)) + lambda3*(mean(theta)-mean_y)^2 }
    }
    
    O1 <- 2/nrow(X) * G %*% t(X) %*% y
    O2 <- 2/nrow(X) * G %*% t(X_u)
    O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
    
    if (lambda2>0) {
      O4<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% t(F) %*% y
      O5<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% C %*% t(X_u)
    }
    
    opt_grad <- function(theta) {
      theta<-matrix(theta)
      reg3<-lambda3*2*(rep(sum(theta),nrow(theta))-rep(nrow(theta)*mean_y,nrow(theta)))

      if (lambda2>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta + reg3)
      else return(O1 + O2 %*% theta - O3 + reg3  ) 
    }
    
    theta <- rep(1.5,nrow(X_u))
    # Bounded optimization
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1))
    theta<-opt_result$par
    
    unlabels<-theta
  }
  
  theta <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1)))) ) %*% (t(Xe) %*% c(y,theta))
  
  new("ICLeastSquaresClassifier",
      classnames=classnames,
      modelform=modelform,
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      intercept=intercept,
      optimization=opt_result
      )
}
