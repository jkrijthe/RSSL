#' @include Classifier.R
setClass("LogisticRegression", 
         representation(w="numeric",intercept="ANY",scaling="ANY"), 
         prototype(name="LogisticRegression"), 
         contains="Classifier")

LogisticRegression <- function(X, y, lambda=0, intercept=TRUE, scale=FALSE, init=NA,x_center=FALSE, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")

  opt_func <- function(w, X, y) {
    w <- matrix(w,nrow=ncol(X))
  
    # Multiclass:
    expscore <- cbind(rep(0,nrow(X)), X %*% w) # Numerators of the probability estimates
  
    ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
  
    for (c in 1:length(classnames)) {
      ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
    }

    return(as.numeric(ll + lambda * w[-1] %*% w[-1]))
  }
  
  opt_grad <- function(w, X, y) {
    
    w <- matrix(w,nrow=ncol(X))
    
    # Two-class
    #t(y-(1-1/(1+exp(X %*% w)))) %*% X
    
    # Multi-class
    expscore <- cbind(rep(0,nrow(X)), X %*% w) # Numerators of the probability estimates    
    
    for (c in 2:length(classnames)) {
      w[,c-1] <- matrix(colSums(X[y==c,,drop=FALSE]), ncol(X),1) - (t(X) %*% (exp(expscore[,c]) / rowSums(exp(expscore))))
    }
    as.vector(w)
  }
  
  if (is.na(init[1])) {
    w <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else {
    w<-init
  }
  
  opt_result <- optim(w, fn=opt_func,gr=opt_grad, X=X, y=y, method="BFGS", control=list(fnscale=-1))
  # w<-as.numeric(opt_result[1,1:length(w)])
  w<-opt_result$par
  return(new("LogisticRegression", modelform=modelform, classnames=classnames, w=w,intercept=intercept,scaling=scaling))
  
}

# Train the Logistic Regression using R's standard glm function
LogisticRegressionFast<-function(modelform,D) {
  object<-glm(formula = modelform, family = binomial("logit"), data = D)
  return(new("LogisticRegression", modelform=modelform, D=D, w=object$coefficients))
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
  
  ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
  
  for (c in 1:length(object@classnames)) {
    ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
  }
  return(-ll)
})                                                                              


#' Log likelihood on the training set                                                                              
# setMethod("logLik", signature(object="LogisticRegression"), function(object) {
#   loss(object,object@D)
# })

#' @rdname predict-methods
#' @aliases predict,LogisticRegression-method  
setMethod("predict", signature(object="LogisticRegression"), function(object, newdata,probs=FALSE) {
ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept)
  X<-ModelVariables$X


  w <- matrix(object@w, nrow=ncol(X))
  expscore <- exp(cbind(rep(0,nrow(X)), X %*% w))
  probabilities <- expscore/rowSums(expscore)
  # If we need to return classes
  classes <- factor(apply(probabilities,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  if (probs)
 {
    return(probabilities)
  } else return(classes)
})

#' @rdname boundaryplot-methods
#' @aliases boundaryplot,LogisticRegression-method  
setMethod("boundaryplot", signature(object="LogisticRegression"), function(object, p) {
  p+geom_abline(intercept = (-(object@w[1])/object@w[3]), slope = (-object@w[2]/object@w[3]))
})
