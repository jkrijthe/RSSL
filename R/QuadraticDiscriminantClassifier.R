# Formal Class Definition
setClass("QuadraticDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Quadratic Discriminant Classifier"),
         contains="NormalBasedClassifier")

# Constructor method: XY
QuadraticDiscriminantClassifierXY <- function(X, y, method="closedform",prior=NULL, scale=FALSE,  ...) {
  if (scale) {
    library(pls)
    scaling<-stdize(X, center = TRUE, scale = TRUE)
    X<-predict(scaling,X)
  } else {scaling=NULL}
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
    #Set priors if not set by user
    if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
    
    #Calculate means for classes
    means<-t((t(X) %*% Y))/(colSums(Y))
    
    #Set sigma to be the average within scatter matrix
    sigma<-lapply(1:ncol(Y),function(c,X){cov(X[Y[,c]==1,,drop=FALSE])},X)
    
  } else if (method=="ml") {
    opt_func<-function(theta, X, y) {
      means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
      sigma<-matrix(theta[((ncol(Y)*ncol(X))+1):length(theta)],ncol(X),ncol(X))
      
      
      sigma<-lapply(1:ncol(Y),function(c){sigma})
      
      model<-new("LinearDiscriminantClassifier", prior=prior, means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
      
      loss(model,X,y)
    }
    
    theta<-rep(0.01,ncol(Y)*ncol(X)+2 * (ncol(X)^2))
    opt_result <- optim(theta, opt_func, gr=NULL, X, y, method="L-BFGS-B", lower=c(-Inf,-Inf,0.000000001),control=list(trace=1,maxit=1000))
    theta<-opt_result$par
    
    means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
    sigma<-theta[(ncol(Y)*ncol(X))+1]
    sigma<-diag(ncol(X))*sigma
    sigma<-lapply(1:ncol(Y),function(c){sigma})
  }
  new("QuadraticDiscriminantClassifier", prior=prior, means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

# Constructor method: formula
# Removes intercept
QuadraticDiscriminantClassifier <- function(model, D, method="closedform", prior=NULL, scale=FALSE) {  
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
  
  # Fit model
  trained<-QuadraticDiscriminantClassifierXY(X, y, method=method,prior=prior,scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}