# Formal class definition
setClass("EntropyRegularizedLogisticRegression",
         representation(theta="numeric"),
         prototype(name="Entropy Regularized Logistic Regression"), 
         contains="LogisticRegression")

# Constructor
EntropyRegularizedLogisticRegressionXY <- function(X,y,X_u,lambda2=1.0,lambda=0.0,init=NA) {

  classnames <- 1:length(unique(y))
  m<-ncol(X)
  
  opt_func <- function(theta,X,y,X_u) {
#     object<-new("LogisticRegression", theta=theta)
#     return(loss(object,X,y) + lambda * object@theta %*% object@theta)
    theta <- matrix(theta,nrow=ncol(X))
    
    # Likelihood term
    expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Denominator of the probability estimates
    ll <- sum(-log(rowSums(exp(expscore)))) # Numerator of the probability estimates, summed
    
    for (c in 1:length(classnames)) {
      ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
    }
    
    # Entropy term
    # Sum over all unlabelled points and classes, take proabbility times log probability
    expscore <- cbind(rep(0,nrow(X_u)), X_u %*% theta)
    sum_exp<-rowSums(exp(expscore))
    
    ent<-0
    for (c in 1:length(classnames)) {
      ent <- ent + sum( (exp(expscore[,c])/sum_exp) * (expscore[,c]-log(sum_exp))) # Sum the numerators for each class
    }
    #print(ll+ent)
    if (is.nan(ll+ent)) return(0.0)
    if (is.infinite(ll+ent)) return(0.0)
    
    return(ll/nrow(X) + lambda2*ent/nrow(X_u) + lambda * matrix(theta,nrow=1) %*% t(matrix(theta,nrow=1)))
    
    
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
  
  opt_result <- optim(theta, opt_func, gr=NULL, X, y, X_u, method="BFGS", control=list(fnscale=-1))
  print(opt_result$par)
  theta<-opt_result$par
  
  new("EntropyRegularizedLogisticRegression",
      classnames=classnames,
      theta=theta)
}

EntropyRegularizedLogisticRegression<-function(modelform,D,lambda2=1.0,lambda=0.0,init=NA) {
  list2env(SSLDataFrameToMatrices(modelform,D,intercept=TRUE),environment())
  
  trained<-EntropyRegularizedLogisticRegressionXY(X,y, init=init, lambda2=lambda2, lambda=lambda)
  trained@modelform<-modelform
  trained@classnames<-classnames
  return(trained)
}