#' @include Classifier.R
setClass("LogisticRegression", 
         representation(w="numeric",
                        intercept="ANY",
                        scaling="ANY",
                        opt_result="ANY"), 
         prototype(name="LogisticRegression",opt_result=NULL), 
         contains="Classifier")

#' (Regularized) Logistic Regression implementation 
#' 
#' Implementation of Logistic Regression that is useful for comparisons with semi-supervised logistic regression implementations, such as \code{\link{EntropyRegularizedLogisticRegression}}.
#' 
#' @family RSSL classifiers
#' 
#' @param lambda numeric; L2 regularization parameter
#' @param init numeric; Initialization of parameters for the optimization
#' @inheritParams BaseClassifier
#' 
#' @export
LogisticRegression <- function(X, y, lambda=0.0, intercept=TRUE, scale=FALSE, init=NA, x_center=FALSE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")

  # Initialization
  if (is.na(init[1])) {
    w <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else {
    w<-init
  }
  
  # Optimization
  opt_result <- optim(w, 
                      fn=loss_logisticregression,
                      gr=grad_logisticregression, 
                      X=X, y=y, lambda=lambda,
                      classnames=classnames,
                      intercept=intercept,
                      method="BFGS", 
                      control=list(fnscale=-1))
  
  w<-opt_result$par
  
  return(new("LogisticRegression", 
             modelform=modelform, 
             classnames=classnames, 
             intercept=intercept,
             scaling=scaling,
             w=w,
             opt_result=opt_result))
  
}

#' Logistic Regression implementation that uses R's glm
#' @param lambda numeric; not used
#' @param init numeric; not used
#' @inheritParams BaseClassifier
#' @export
LogisticRegressionFast <- function(X, y, lambda=0, intercept=TRUE, scale=FALSE, init=NA, x_center=FALSE) {
  
  stopifnot(lambda==0)
  
  ModelVariables<-PreProcessing(X,y,
                                scale=scale,
                                intercept=intercept,
                                x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform

  object <- glm.fit(X, y, family = binomial("logit"),intercept=FALSE)

    return(new("LogisticRegression", 
             modelform=modelform, 
             classnames=classnames, 
             w=object$coefficients,
             intercept=intercept,
             scaling=scaling))
}

# Log likelihood on a new data matrix X and outcome vector y
# setMethod("logLikelihood", signature(object="LogisticRegression", x="matrix"), function(object, x, y) {
#   X <- x
#   w <- matrix(object@w,nrow=ncol(X))
  
#   # Multiclass:
#   expscore <- cbind(rep(0,nrow(X)), X %*% w) # Numerators of the probability estimates
  
#   ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
  
#   for (c in 1:length(object@classnames)) {
#     ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
#   }
#   return(ll)
# })

#' @rdname loss-methods
#' @aliases loss,LogisticRegression-method           
setMethod("loss", signature(object="LogisticRegression"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=object@intercept)
  X<-ModelVariables$X
  y<-ModelVariables$y

  if (is.null(y)) { stop("No labels supplied.")}
  
  w <- matrix(object@w,nrow=ncol(X))
  
  # Multiclass:
  expscore <- cbind(rep(0,nrow(X)), X %*% w) # Numerators of the probability estimates
  
  ll <- -log(rowSums(exp(expscore))) # Denominators of the probability estimates
  
  for (c in 1:length(object@classnames)) {
    ll[y==object@classnames[c]] <- ll[y==object@classnames[c]] + expscore[y==object@classnames[c],c] # Sum the numerators for each class
  }
  return(-ll)
})

#' @rdname rssl-predict
#' @aliases predict,LogisticRegression-method  
setMethod("predict", signature(object="LogisticRegression"), function(object, newdata) {
ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept)
  X<-ModelVariables$X

  w <- matrix(object@w, nrow=ncol(X))
  expscore <- exp(cbind(rep(0,nrow(X)), X %*% w))
  probabilities <- expscore/rowSums(expscore)
  
  # If we need to return classes
  classes <- factor(apply(probabilities,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  return(classes)
})

#' @rdname posterior-methods
#' @aliases posterior,LogisticRegression-method
setMethod("posterior", signature(object="LogisticRegression"), function(object,newdata) {
  
  ModelVariables<-PreProcessingPredict(modelform=object@modelform,
                                       newdata=newdata,
                                       y=NULL,
                                       scaling=object@scaling,
                                       intercept=object@intercept)
  
  X<-ModelVariables$X
  
  w <- matrix(object@w, nrow=ncol(X))
  expscore <- exp(cbind(rep(0,nrow(X)), X %*% w))
  posteriors <- expscore/rowSums(expscore)
  
  colnames(posteriors) <- object@classnames
  return(posteriors)
})

#' @rdname line_coefficients-methods
#' @aliases line_coefficients,LogisticRegression-method 
setMethod("line_coefficients", signature(object="LogisticRegression"), function(object) {
  return(coefficients_after_scaling(w0=object@w[1]-(0.0),w=object@w[2:3],scaling=object@scaling))
})

loss_logisticregression <- function(w, X, y, classnames, lambda,intercept=TRUE) {
  w <- matrix(w,nrow=ncol(X))
  
  # Multiclass:
  expscore <- cbind(rep(0,nrow(X)), X %*% w) # Numerators of the probability estimates
  
  ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
  
  for (c in 1:length(classnames)) {
    ll <- ll + sum(expscore[y==classnames[c],c]) # Sum the numerators for each class
  }
  if (intercept)  return(as.numeric(ll - lambda * w[-1] %*% w[-1]))
  else return(as.numeric(ll - lambda * t(w) %*% w))
}

grad_logisticregression <- function(w, X, y, classnames, lambda,intercept=TRUE) {
  w <- matrix(w,nrow=ncol(X))
  
  # Two-class
  #t(y-(1-1/(1+exp(X %*% w)))) %*% X
  
  # Multi-class
  expscore <- cbind(rep(0,nrow(X)), X %*% w) # Numerators of the probability estimates    
  
  grad <- matrix(w,nrow=ncol(X))
  for (cu_class in 2:length(classnames)) {
    grad[,cu_class-1] <- matrix(colSums(X[y==classnames[cu_class],,drop=FALSE]), ncol(X),1) + 
      - (t(X) %*% (exp(expscore[,cu_class]) / rowSums(exp(expscore))))
  }
  
  if (intercept) return(as.numeric(grad - 2* lambda* c(0,w[-1,])))
  else return(as.numeric(grad - 2* lambda* w))
}
