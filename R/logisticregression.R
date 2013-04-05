# Generics TODO: MOVE
setGeneric("logLikelihood",
           function(object, x, ...) standardGeneric("logLikelihood")
)

# Formal class definition
setClass("LogisticRegression", 
         representation(theta="numeric"), 
         prototype(name="LogisticRegression"), 
         contains="Classifier")

LogisticRegressionXY<-function(X,y, init=NA, lambda=0.0) {
  classnames<-1:length(unique(y))
  opt_func <- function(theta, X, y) {
    object<-new("LogisticRegression", theta=theta)
    return(loss(object,X,y) + lambda * object@theta %*% object@theta)
  }
  
  opt_grad <- function(theta, X,y) {
    
    theta <- matrix(theta,nrow=ncol(X))
    
    # Two-class
    #t(y-(1-1/(1+exp(X %*% theta)))) %*% X
    
    # Multi-class
    expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates    
    
    for (c in 2:length(classnames)) {
      theta[,c-1] <- matrix(colSums(X[y==c,,drop=FALSE]), ncol(X),1) - (t(X) %*% (exp(expscore[,c]) / rowSums(exp(expscore))))
    }
    as.vector(theta)
  }
  
  if (is.na(init[1])) {
    theta <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else {
    theta<-init
  }
  
  opt_result <- optim(theta, opt_func, gr=opt_grad, X, y, method="BFGS", control=list(fnscale=-1))
  theta<-opt_result$par
  return(new("LogisticRegression", classnames=classnames, theta=theta))
  
}

#' Constructor Method
#' Note: the first class is the reference class
LogisticRegression<-function(modelform, D, init=NA,lambda=0.0) {
  list2env(SSLDataFrameToMatrices(modelform,D,intercept=TRUE),environment())
  
  trained<-LogisticRegressionXY(X,y, init=init, lambda=lambda)
  trained@modelform<-modelform
  trained@classnames<-classnames
  return(trained)
}

# Train the Logistic Regression using R's standard glm function
LogisticRegressionFast<-function(modelform,D) {
  object<-glm(formula = modelform, family = binomial("logit"), data = D)
  return(new("LogisticRegression", modelform=modelform, D=D, theta=object$coefficients))
}

# Log likelihood on a new data matrix X and outcome vector y
setMethod("logLikelihood", signature(object="LogisticRegression", x="matrix"), function(object, x, y) {
  X <- x
  theta <- matrix(object@theta,nrow=ncol(X))
  
  # Multiclass:
  expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates
  
  ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
  
  for (c in 1:length(object@classnames)) {
    ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
  }
  return(ll)
})
          
#' Log likelihood on a new data set                                                                                                 
setMethod("loss", signature(object="LogisticRegression"), function(object, newdata, y=NULL) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    if (is.null(y)) { stop("No labels supplied.")}
    X<-newdata
  }
  
  
  theta <- matrix(object@theta,nrow=ncol(X))
  
  # Multiclass:
  expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates
  
  ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
  
  for (c in 1:length(object@classnames)) {
    ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
  }
  return(ll)
})                                                                              


#' Log likelihood on the training set                                                                              
setMethod("logLik", signature(object="LogisticRegression"), function(object) {
  loss(object,object@D)
})

#' Predict
setMethod("predict", signature(object="LogisticRegression"), function(object, newdata,probs=FALSE) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- exp(cbind(rep(0,nrow(X)), X %*% theta))
  probabilities <- expscore/rowSums(expscore)
  # If we need to return classes
  classes <- factor(apply(probabilities,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  if (probs)
 {
    return(probabilities)
  } else return(classes)
})