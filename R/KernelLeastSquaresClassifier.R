#' @include Classifier.R
setClass("KernelLeastSquaresClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY",intercept="ANY",kernel="ANY",Xtrain="ANY"),
         prototype(name="LeastSquaresClassifier",scaling=NULL,kernel=NULL,Xtrain=NULL), 
         contains="Classifier")

#' Least Squares Classifier
#'
#' Use least squares regression as a classification technique using classes as targets (1 for one class, 2 for the other). Implemented using matrix inversions, not the more numerically stable Singular Value Decomposition method. Note this method minimizes quadratic loss, not the truncated quadratic loss.
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
KernelLeastSquaresClassifier <- function(X, y, lambda=0, kernel=NULL, intercept=TRUE, x_center=FALSE, scale=FALSE, method="inverse",...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  # Check number of classes
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  # Check if all classes are present.
  
  #There is a problem using ginv when using PCs as inputs: the problems seem to be rescaled such that the bias term is no longer correct
  
  ## Start Implementation
  n<-nrow(X)
  m<-ncol(X)
  
  Xtrain<-NULL
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  objective<-function(w,X,y) {
    sum((X%*%w-y)^2)
  }
  gradient<-function(w,X,y) {
    2 * t(X) %*% X %*% w - 2 * t(X) %*% y
  }
  hessian<-function(w,X,y) {
    2 * t(X) %*% X
  }
  
  if (method=="inverse") {
    if (is.null(kernel)) {
      if (intercept) {
        theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% y)
      } else {
        theta <- inv(t(X) %*% X + n*lambda*diag(rep(1,m))) %*% (t(X) %*% y)
      }
    } else {
      require(kernlab)
      if (inherits(kernel,"kernel")) {
        Xtrain<-X
        K<-kernelMatrix(kernel,X,X)
        theta <- inv(K+lambda*diag(n)) %*% (y-mean(y))
      }    
    }
  } else if (method=="Normal") {
    solution <- solve(t(X) %*% X, t(X) %*% y)
    theta<-matrix(solution)
  }  else if (method=="QR") {
    solution <- qr.solve(X,y)
    theta<-matrix(solution)
  } else if (method=="QR2") {
    solution <- qr.solve(t(X) %*% X, t(X) %*% y)
    theta<-matrix(solution)
  } else if (method=="BFGS") {
    # BFGS gradient descent
    theta<-rep(0,ncol(X))
    theta<-matrix(optim(theta,fn=objective,gr=gradient,X=X,y=y,method="BFGS")$par)     
  } else if (method=="BFGSCPP") {
    # BFGS gradient descent
    theta<-rep(0,ncol(X))
    theta<-matrix(optim(theta,fn=function(w,X,y) { squared_objective(matrix(w),X,matrix(y)) },gr=function(w,X,y) { squared_gradient(matrix(w),X,matrix(y)) },X=X,y=y,method="BFGS")$par)
  } 
  else if (method=="CPP") {
    # Stochastic gradient descent
    theta<-squared_solution(X,matrix(y))
  } else if (method=="SGD") {
    # Stochastic gradient descent
  }else if (method=="CG") {
    # Conjugate gradient method
    theta <- rep(0,ncol(X))
    theta <- matrix(optim(theta,fn=objective,gr=gradient,X=X,y=y,method="CG")$par)
  } else if (method=="Newton") {
    
    returnfunction<-function(w,X,y) {
      val<-objective(w,X,y)
      attr(val,"gradient")<-gradient(w,X,y)
      attr(val,"hessian")<-hessian(w,X,y)
      return(val)
    }
    theta <- rep(0,ncol(X))
    theta<-matrix(nlm(returnfunction,p=theta, X=X,y=y,iterlim=1000)$estimate)
  } else {
    stop("Unknown method")
  }
  
  ## Return correct object
  new("KernelLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      intercept=intercept,
      kernel=kernel,
      Xtrain=Xtrain
  )
}

#' @rdname loss-methods
#' @aliases loss,LeastSquaresClassifier-method
#' @export
setMethod("loss", signature(object="KernelLeastSquaresClassifier"), function(object, newdata, y=NULL,...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=object@intercept)
  X<-ModelVariables$X
  y<-ModelVariables$y
  if (is.null(y)) { stop("No labels supplied.")}
  return(((X %*% object@theta - y)^2))
})

#' @rdname predict-methods
#' @aliases predict,LeastSquaresClassifier-method
setMethod("predict", signature(object="KernelLeastSquaresClassifier"), function(object, newdata, probs=FALSE,...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept)
  
  X<-ModelVariables$X
  
  if (is.null(object@kernel)) {
    theta <- matrix(object@theta, nrow=ncol(X))
    expscore <- X %*% theta
  } else {
    expscore<-kernelMatrix(object@kernel,X,object@Xtrain)%*% object@theta
  }
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore>1.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  
  if (probs){
    return(expscore)
  } else {
    return(classes)
  }
})

#' @rdname plot-methods
#' @aliases plot,LeastSquaresClassifier,missing-method
setMethod("plot", signature(x="KernelLeastSquaresClassifier",y="missing"), function(x) {
  object<-x
  #p<-qplot(object@D[is.na(object@D[,object@classname]),1],object@D[is.na(object@D[,object@classname]),2],color=object@unlabels)
  p<-qplot(object@D[,1],object@D[,2],color=object@D[,object@classname])
  p<-p+geom_abline(intercept = (1.5-x@theta[1])/x@theta[3], slope = -x@theta[2]/x@theta[3])
  return(p)
})

#' @rdname boundaryplot-methods
#' @aliases boundaryplot,LeastSquaresClassifier-method
setMethod("boundaryplot", signature(object="KernelLeastSquaresClassifier"), function(object, p) {
  p+geom_abline(intercept = (-(object@theta[1]-1.5)/object@theta[3]), slope = (-object@theta[2]/object@theta[3]))
})

