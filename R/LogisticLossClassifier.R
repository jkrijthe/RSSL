#' LogisticLossClassifier
#' @include Classifier.R
setClass("LogisticLossClassifier", 
         representation(w="numeric"), 
         prototype(name="LogisticLossClassifier"), 
         contains="Classifier")

#' Logistic Loss Classifier
#'
#' Find the linear classifier which minimizing the logistic loss on the training set, optionally using L2 regularization.
#'
#' @family RSSL classifiers
#' 
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector with class assignments
#' @param lambda Regularization parameter used for l2 regularization
#' @param intercept TRUE if an intercept should be added to the model
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param init Starting parameter vector for gradient descent
#' @param x_center logical; Whether the feature vectors should be centered
#' @param ... additional arguments
#' @return S4 object with the following slots
#' \item{w}{the weight vector of the linear classifier}
#' \item{classnames}{vector with names of the classes}
#' 
#' @export
LogisticLossClassifier <- function(X, y, lambda=0, intercept=TRUE, scale=FALSE, init=NA, x_center=FALSE, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  Y<-ModelVariables$Y[,1,drop=FALSE]
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  y <- (Y-0.5)*2
  
  if (is.na(init[1])) {
    w <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else {
    w<-init
  }
  
#   iterations<-100000
#   alpha<-0.1
#   for (i in 1:iterations) {
#     w <- w - alpha*opt_grad(w,X,y)
#     #print(opt_func(w,X,y))
#   }
  
  opt_result <- optim(w, 
                      fn=loss_logistic, 
                      gr=grad_logistic, 
                      X=X, y=y, lambda=lambda,
                      method="BFGS")
  
  w<-opt_result$par
  
  return(new("LogisticLossClassifier", 
             modelform=modelform, 
             classnames=classnames, 
             w=w, 
             scaling=scaling))
}
                                                                                        
#' @rdname loss-methods
#' @aliases loss,LogisticLossClassifier-method  
setMethod("loss", signature(object="LogisticLossClassifier"), function(object, newdata, y=NULL, ...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=TRUE,classnames = object@classnames)
  X <- ModelVariables$X
  Y <- ModelVariables$Y[,1,drop=FALSE]
  y <- ModelVariables$y
  
  if (is.null(y)) { stop("No labels supplied.")}
  
  y <- (Y[,1]-0.5)*2
  log(matrix(1,nrow(X),1)+exp(- y * (X %*% object@w)))
})                     

#' @rdname rssl-predict
#' @aliases predict,LogisticLossClassifier-method  
setMethod("predict", signature(object="LogisticLossClassifier"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=TRUE)
  
  X<-ModelVariables$X
  
  w <- matrix(object@w, nrow=ncol(X))
  classes <- factor(X %*% w > 0, levels=c(TRUE,FALSE), labels=object@classnames)
  
  return(classes)
})

#' @rdname line_coefficients-methods
#' @aliases line_coefficients,LogisticLossClassifier-method 
setMethod("line_coefficients", signature(object="LogisticLossClassifier"), function(object) {
  return(coefficients_after_scaling(w0=object@w[1],w=object@w[2:3],scaling=object@scaling))
})

loss_logistic <- function(w, X, y, lambda) {
  
  loss <- sum(log(matrix(1,nrow(X),1)+exp(- y * (X %*% w)))) + lambda * w[-1] %*% w[-1]
  
  return(as.numeric(loss))
}

grad_logistic <- function(w, X, y, lambda) {
  w <- matrix(w,nrow=ncol(X))
  # Numerators of the probability estimates    
  weightings <- -y * (exp(- y * (X %*% w))/(matrix(1,nrow(X),1)+exp(- y * (X %*% w))))
  as.vector(t(X) %*% weightings + 2 *lambda *c(0,w[-1]))
}
