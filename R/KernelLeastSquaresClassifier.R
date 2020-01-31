#' @include Classifier.R
setClass("KernelLeastSquaresClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY",intercept="ANY",Xtrain="ANY",y_scale="numeric",kernel="ANY",threshold="numeric"),
         prototype(name="KernelLeastSquaresClassifier",scaling=NULL,kernel=NULL,Xtrain=NULL,y_scale=0,threshold=0.5), 
         contains="Classifier")

#' Kernelized Least Squares Classifier
#'
#' Use least squares regression as a classification technique using a numeric encoding of classes as targets. Note this method minimizes quadratic loss, not the truncated quadratic loss.
#'
#' @family RSSL classifiers
#' 
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param lambda Regularization parameter of the l2 penalty in regularized least squares
#' @param kernel kernlab kernel function
#' @param y_scale TRUE center the target vector
#' @param x_center TRUE, whether the dependent variables (features) should be centered
#' @param scale If TRUE, apply a z-transform to the design matrix X before running the regression
#' 
#' @return S4 object of class LeastSquaresClassifier with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the parameters of the z-transforms applied to the data}
#' 
#' @example inst/examples/example-KernelLeastSquaresClassifier.R
#' @export
KernelLeastSquaresClassifier <- function(X, y, lambda=0, kernel=vanilladot(), x_center=TRUE, scale=TRUE, y_scale=TRUE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  if (ncol(ModelVariables$Y)==2) {
    Y <- ModelVariables$Y[,1,drop=FALSE]
  } else {
    Y <- ModelVariables$Y
  }
  
  ## Start Implementation
  n<-nrow(X)
  m<-ncol(X)
  
  Xtrain<-NULL
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  if (y_scale) {
    y_scale <- colMeans(Y)
  } else {
    y_scale <- rep(0,ncol(Y))
  }
  
  Y <- sweep(Y,2,y_scale) # Possibly center the numeric Y labels
  
  if (inherits(kernel,"kernel")) {
      Xtrain <- X
      K <- kernelMatrix(kernel,X,X)
      theta <- solve(K+lambda*diag(n)*n, Y)
  } else {
    stop("No appropriate kernel function from kernlab supplied. See, for instance, the help of vanilladot()")
  }
  
  ## Return correct object
  new("KernelLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      kernel=kernel,
      Xtrain=Xtrain,
      y_scale=y_scale
  )
}

#' @rdname loss-methods
#' @aliases loss,KernelLeastSquaresClassifier-method
setMethod("loss", signature(object="KernelLeastSquaresClassifier"), function(object, newdata, y=NULL,...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
  X<-ModelVariables$X
  if (ncol(ModelVariables$Y)==2) {
    Y <- ModelVariables$Y[,1,drop=FALSE]
  } else {
    Y <- ModelVariables$Y
  }
  
  if (is.null(Y)) { stop("No labels supplied.")}

  expscore <- kernelMatrix(object@kernel,X,object@Xtrain)%*% object@theta
  Y <- sweep(Y,2,object@y_scale,"-")
  return(rowSums((expscore - Y)^2))
})

#' @rdname rssl-predict
#' @aliases predict,LeastSquaresClassifier-method
setMethod("predict", signature(object="KernelLeastSquaresClassifier"), function(object, newdata,...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=FALSE)
  
  X<-ModelVariables$X
  
  expscore <- kernelMatrix(object@kernel,X,object@Xtrain)%*% object@theta
  expscore <- sweep(expscore,2,object@y_scale,"+")
  
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore<0.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  
  return(classes)
})

#' @rdname decisionvalues-methods
setMethod("decisionvalues", signature(object="KernelLeastSquaresClassifier"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=FALSE)
  
  X<-ModelVariables$X
  expscore <- kernelMatrix(object@kernel,X,object@Xtrain)%*% object@theta
  expscore <- sweep(expscore,2,object@y_scale,"+")
  return(expscore)
})
