#' @include Classifier.R
setClass("LeastSquaresClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY",intercept="ANY"),
         prototype(name="LeastSquaresClassifier",scaling=NULL), 
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
LeastSquaresClassifier <- function(X, y, lambda=0, intercept=TRUE, x_center=TRUE, scale=FALSE, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
  #There is a problem using ginv when using PCs as inputs: the problems seem to be rescaled such that the bias term is no longer correct
  
  ## Start Implementation
  n<-nrow(X)
  m<-ncol(X)
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  if (intercept) {
    theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% y)
  } else {
    theta <- inv(t(X) %*% X + n*lambda*diag(rep(1,m))) %*% (t(X) %*% y)
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

#' Loss method for Least Squares Classifier
#'
#' Loss on new objects of a trained least squares classifier
#'
#' @usage loss(object, X, probs=FALSE)
#' @usage loss(object, newdata, lambda=0, probs=FALSE)
#'
#' @param object Object of class LeastSquaresClassifier
#' @param X Design matrix of the test data, intercept term is added within the function
#' @param y Vector with true classes of the test data
#' @param newdata data.frame object with test data
#' @return numeric of the total loss on the test data
#' @rdname loss-methods
#' @aliases loss,LeastSquaresClassifier-method
setMethod("loss", signature(object="LeastSquaresClassifier"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=object@intercept)
  X<-ModelVariables$X
  y<-ModelVariables$y
  if (is.null(y)) { stop("No labels supplied.")}
  return(sum((X %*% object@theta - y)^2))
})

#' Predict method for Least Squares Classifier
#'
#' Predict classes of new data based on trained least squares classifier
#'
#' @usage predict(object, X, probs=FALSE)
#' @usage predict(object, newdata, lambda=0, probs=FALSE)
#'
#' @param object Object of class LeastSquaresClassifier
#' @param X Design matrix of the test data, intercept term is added within the function
#' @param newdata data.frame object with test data
#' @param probs whether class probabilities should be returned
#' @return factor of predicted classes
#' @rdname predict-methods
#' @aliases predict,LeastSquaresClassifier-method
setMethod("predict", signature(object="LeastSquaresClassifier"), function(object, newdata, probs=FALSE) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept)

  X<-ModelVariables$X
  
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  
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

#' Plot method for LeastSquaresClassifier
#'
#' @param x LeastSquaresClassifier object
#' @rdname plot-methods
#' @aliases plot,LeastSquaresClassifier,missing-method
setMethod("plot", signature(x="LeastSquaresClassifier",y="missing"), function(x) {
  object<-x
  #p<-qplot(object@D[is.na(object@D[,object@classname]),1],object@D[is.na(object@D[,object@classname]),2],color=object@unlabels)
  p<-qplot(object@D[,1],object@D[,2],color=object@D[,object@classname])
  p<-p+geom_abline(intercept = (1.5-x@theta[1])/x@theta[3], slope = -x@theta[2]/x@theta[3])
  return(p)
})

#' Boundary plot method for LeastSquaresClassifier
#'
#' @param object LeastSquaresClassifier object
#' @param p ggplot object of classification problem generated by clplot
#' @rdname boundaryplot-methods
#' @aliases boundaryplot,LeastSquaresClassifier-method
setMethod("boundaryplot", signature(object="LeastSquaresClassifier"), function(object, p) {
  p+geom_abline(intercept = (-(object@theta[1]-1.5)/object@theta[3]), slope = (-object@theta[2]/object@theta[3]))
})
