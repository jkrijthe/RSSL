#' @include Classifier.R
setClass("LeastSquaresClassifier",
         representation(theta="matrix",scaling="ANY",optimization="ANY",intercept="ANY",y_scale="numeric",threshold="numeric"),
         prototype(name="LeastSquaresClassifier",scaling=NULL,y_scale=0,threshold=0.5), 
         contains="Classifier")

#' Least Squares Classifier
#'
#' Classifier that minimizes the quadratic loss or, equivalently, least squares regression applied to a numeric encoding of the class labels as target. Note this method minimizes quadratic loss, not the truncated quadratic loss. Optionally, L2 regularization can be applied by setting the \code{lambda} parameter.
#'
#' @family RSSL classifiers
#' 
#' @param lambda Regularization parameter of the l2 penalty
#' @param intercept TRUE if an intercept should be added to the model
#' @param x_center TRUE, whether the dependent variables (features) should be centered
#' @param scale If TRUE, apply a z-transform to the design matrix X before running the regression
#' @param y_scale If True scale the target vector
#' @param method Method to use for fitting. One of c("inverse","Normal","QR","BFGS")
#' @inheritParams BaseClassifier
#' @return S4 object of class LeastSquaresClassifier with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the parameters of the z-transforms applied to the data}
#' @export
LeastSquaresClassifier <- function(X, y, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, method="inverse", y_scale=FALSE) {
  
  if (!is.numeric(lambda) | lambda<0) { stop("Incorrect alpha.") }
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  if (ncol(ModelVariables$Y)==2) {
    Y <- ModelVariables$Y[,1,drop=FALSE]
  } else {
    Y <- ModelVariables$Y
  }
  
  ## Start Implementation
  n <- nrow(X)
  m <- ncol(X)
  k <- ncol(Y)
  
  if (y_scale) {
    y_scale <- colMeans(Y)
  } else {
    y_scale <- rep(0,ncol(Y))
  }
  
  Y <- sweep(Y,2,y_scale) # Possibly center the numeric Y labels
  
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  if (method=="inverse") {
      if (intercept) {
        theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% Y)
      } else {
        theta <- inv(t(X) %*% X + n*lambda*diag(rep(1,m))) %*% (t(X) %*% Y)
      }
  } else if (method=="Normal") {
    if (intercept) solution <- solve(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1)))), t(X) %*% Y)
    else solution <- solve(t(X) %*% X + n*lambda*diag(m), t(X) %*% Y)
    theta<-matrix(solution,m,k)
  }  else if (method=="QR") {
    solution <- qr.solve(X,Y)
    theta<-matrix(solution,m,k)
  } else if (method=="QR2") {
    if (intercept) solution <- qr.solve(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1)))), t(X) %*% Y)
    else solution <- qr.solve(t(X) %*% X + n*lambda*diag(m), t(X) %*% Y)
    theta<-matrix(solution,m,k)
  } else if (method=="BFGS") {
    # BFGS gradient descent
    theta<-rep(0,m*k)
    theta<-matrix(solve_quadratic_bfgs(X,Y,lambda),m,k)     
  } else if (method=="BFGSCPP") {
    # BFGS gradient descent
    theta<-rep(0,ncol(X))
    theta<-matrix(optim(theta,fn=function(w,X,Y) { squared_objective(matrix(w),X,matrix(Y)) },gr=function(w,X,Y) { squared_gradient(matrix(w),X,matrix(Y)) },X=X,Y=Y,method="BFGS")$par,m,k)
  } else if (method=="CG") {
    # Conjugate gradient method
    theta <- rep(0,ncol(X))
    theta <- matrix(optim(theta,fn=squared_objective,gr=squared_gradient,X=X,y=Y,method="CG")$par)
  } else if (method=="Newton") {
    
    returnfunction<-function(w,X,Y) {
      val<-squared_objective(w,X,Y)
      attr(val,"gradient")<-squared_gradient(w,X,Y)
      attr(val,"hessian")<-squared_hessian(w,X,Y)
      return(val)
    }
    theta <- rep(0,ncol(X))
    theta<-matrix(nlm(returnfunction,p=theta, X=X,Y=Y,iterlim=1000)$estimate)
  } else {
    stop("Unknown method")
  }
  
  ## Return correct object
  new("LeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      y_scale=y_scale
      )
}

#' @rdname loss-methods
#' @aliases loss,LeastSquaresClassifier-method
#' @export
setMethod("loss", signature(object="LeastSquaresClassifier"), function(object, newdata, y=NULL,...) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  if (ncol(ModelVariables$Y)==2) {
    Y <- ModelVariables$Y[,1,drop=FALSE]
  } else {
    Y <- ModelVariables$Y
  }

  if (is.null(Y)) { stop("No labels supplied.")}
  
  return(rowSums((decisionvalues(object,newdata) - Y)^2))
})

#' @rdname rssl-predict
#' @aliases predict,LeastSquaresClassifier-method
setMethod("predict", signature(object="LeastSquaresClassifier"), function(object, newdata, ...) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  expscore <- sweep(expscore,2,object@y_scale,"+")
  # If we need to return classes
  if (ncol(theta)>1) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore[,1]<0.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  
  return(classes)
})

#' @rdname decisionvalues-methods
#' @aliases decisionvalues,LeastSquaresClassifier-method
setMethod("decisionvalues", signature(object="LeastSquaresClassifier"), function(object, newdata) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  expscore <- sweep(expscore,2,object@y_scale,"+")
  
  return(expscore)
})

#' @rdname line_coefficients-methods
#' @aliases line_coefficients,LeastSquaresClassifier-method 
setMethod("line_coefficients", signature(object="LeastSquaresClassifier"), function(object) {
  return(coefficients_after_scaling(w0=object@theta[1]-(0.5-object@y_scale),w=object@theta[2:3],scaling=object@scaling))
})

squared_objective <- function(w,X,y) {
  w <- matrix(w,nrow=ncol(X))
  sum((X%*%w-y)^2)
}


squared_gradient<-function(w,X,y) {
  w <- matrix(w,nrow=ncol(X))
  2 * t(X) %*% X %*% w - 2 * t(X) %*% y
}

squared_hessian<-function(w,X,y) {
  2 * t(X) %*% X
}

solve_quadratic_bfgs <- function(X,y,lambda) {
  warning("No regularization implemented.")
  theta <- rep(0.0,ncol(X))
  optim(theta,fn=squared_objective,gr=squared_gradient,X=X,y=y,method="BFGS")$par
}
