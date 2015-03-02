#' @include Classifier.R
setClass("LeastSquaresClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY",intercept="ANY",y_scale="numeric"),
         prototype(name="LeastSquaresClassifier",scaling=NULL,y_scale=0), 
         contains="Classifier")

#' Least Squares Classifier
#'
#' Use least squares regression as a classification technique using class indicators as targets. Note this method minimizes quadratic loss, not the truncated quadratic loss.
#'
#' @usage LeastSquaresClassifier(X, y, lambda=0, intercept=TRUE, x_center, scale=FALSE, ...)
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param lambda Regularization parameter of the l2 penalty in regularized least squares
#' @param intercept TRUE if an intercept should be added to the model
#' @param x_center TRUE, whether the dependent variables (features) should be centered
#' @param scale If TRUE, apply a z-transform to the design matrix X before running the regression
#' @param ... additional arguments
#' @return S4 object of class LeastSquaresClassifier with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' @export
LeastSquaresClassifier <- function(X, y, lambda=0, intercept=TRUE, x_center=FALSE, scale=FALSE, method="inverse", y_scale=FALSE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  Y <- ModelVariables$Y
  
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
  } 
  else if (method=="CPP") {
    theta<-squared_solution(X,matrix(Y))
  } else if (method=="CG") {
    # Conjugate gradient method
    theta <- rep(0,ncol(X))
    theta <- matrix(optim(theta,fn=objective,gr=gradient,X=X,y=Y,method="CG")$par)
  } else if (method=="Newton") {
    
    returnfunction<-function(w,X,Y) {
      val<-objective(w,X,Y)
      attr(val,"gradient")<-gradient(w,X,Y)
      attr(val,"hessian")<-hessian(w,X,Y)
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
      intercept=intercept
      )
}

#' @rdname loss-methods
#' @aliases loss,LeastSquaresClassifier-method
#' @export
setMethod("loss", signature(object="LeastSquaresClassifier"), function(object, newdata, y=NULL,...) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  Y <- ModelVariables$Y

  if (is.null(Y)) { stop("No labels supplied.")}
  Y <- sweep(Y,2,object@y_scale,"-")
  
  return(rowSums((X %*% object@theta - Y)^2))
})

#' @rdname predict-methods
#' @aliases predict,LeastSquaresClassifier-method
setMethod("predict", signature(object="LeastSquaresClassifier"), function(object, newdata, probs=FALSE,...) {
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
  
  if (probs){
    return(expscore)
  } else {
    return(classes)
  }
})

setMethod("decisionvalues", signature(object="LeastSquaresClassifier"), function(object, newdata) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  expscore <- sweep(expscore,2,object@y_scale,"+")
  
  return(expscore)
})

#' @rdname plot-methods
#' @aliases plot,LeastSquaresClassifier,missing-method
setMethod("show", signature(object="LeastSquaresClassifier"), function(object) {
  print(object@theta)
})

#' @rdname plot-methods
#' @aliases plot,LeastSquaresClassifier,missing-method
setMethod("plot", signature(x="LeastSquaresClassifier",y="missing"), function(x) {
  object<-x
  #p<-qplot(object@D[is.na(object@D[,object@classname]),1],object@D[is.na(object@D[,object@classname]),2],color=object@unlabels)
  p<-qplot(object@D[,1],object@D[,2],color=object@D[,object@classname])
  p<-p+geom_abline(intercept = (1.5-x@theta[1])/x@theta[3], slope = -x@theta[2]/x@theta[3])
  return(p)
})

#' @rdname boundaryplot-methods
#' @aliases boundaryplot,LeastSquaresClassifier-method
setMethod("boundaryplot", signature(object="LeastSquaresClassifier"), function(object, p) {
  p+geom_abline(intercept = (-(object@theta[1]-0.5)/object@theta[3]), slope = (-object@theta[2]/object@theta[3]))
})

solve_quadratic_bfgs <- function(X,y,lambda) {
  warning("No regularization implemented.")
  
  objective<-function(w,X,y) {
    w <- matrix(w,m,k)
    sum((X%*%w-y)^2)
  }
  gradient<-function(w,X,y) {
    w <- matrix(w,m,k)
    2 * t(X) %*% X %*% w - 2 * t(X) %*% y
  }
  hessian<-function(w,X,y) {
    2 * t(X) %*% X
  }
  
  optim(theta,fn=objective,gr=gradient,X=X,y=y,method="BFGS")$par
}
