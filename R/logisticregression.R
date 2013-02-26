# Generics TODO: MOVE
setGeneric("logLikelihood",
           function(object, x, ...) standardGeneric("logLikelihood")
)

# Formal class definition
setClass("LogisticRegression", 
         representation(theta="numeric",X="matrix",y="integer"), 
         prototype(name="LogisticRegression"), 
         contains="Classifier")

#' Constructor Method
#' Note: the first class is the reference class
LogisticRegression<-function(modelform, D, init=NA,lambda=0.0) {
  D_trans <- model.frame(modelform, D)
  X<-model.matrix(modelform, D)
  y<-as.integer(D_trans[,1])
  classnames <- levels(D_trans[,1])
  
  opt_func <- function(theta, X, y) {
    
    object<-new("LogisticRegression", modelform=modelform, classnames=classnames, D=D, y=y, X=X, theta=theta)
    return(logLikelihood(object,X,y) + lambda * object@theta %*% object@theta)
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
  return(new("LogisticRegression", modelform=modelform, classnames=classnames, D=D, theta=theta))
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
setMethod("logLikelihood", signature(object="LogisticRegression", x="data.frame"), function(object, x) {
  theta <- matrix(object@theta,nrow=ncol(x))
  D_trans <- model.frame(object@modelform, x)
  X <- model.matrix(object@modelform, x)
  y <- as.integer(D_trans[,1])
  
  # Multiclass:
  expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates
  
  ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
  
  for (c in 1:length(object@classnames)) {
    ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
  }
  return(ll)
})                                                                              

#' Log likelihood on the training set                                                                              
setMethod("logLikelihood", signature(object="LogisticRegression",x="missing"), function(object) {
  logLikelihood(object,object@D)
})   

#' Log likelihood on the training set                                                                              
setMethod("logLik", signature(object="LogisticRegression"), function(object) {
  logLikelihood(object,object@D)
})

#' Predict
setMethod("predict", signature(object="LogisticRegression"), function(object, D, probs=FALSE) {
  
  D[,object@classname] <- 1
  X<-model.matrix(modelform,D)
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