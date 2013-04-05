# Formal Class Definition
setClass("MCLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Moment Constrained Linear Discriminant Classifier"),
         contains="NormalBasedClassifier")

# Constructor method: XY
MCLinearDiscriminantClassifierXY <- function(X, y, X_u, method="closedform",prior=NULL, scale=FALSE,  ...) {
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
    sigma.classes<-lapply(1:ncol(Y),function(c,X){cov(X[Y[,c]==1,])},X)
    sigma<-sigma.classes[[1]]*prior[1]
    for (i in 2:length(sigma.classes)) {
      sigma<-sigma+sigma.classes[[i]]*prior[i]
    }
    
    
    T.labeled<-cov(X)
    T.all<-cov(rbind(X,X_u))
    m.labeled<-colMeans(X)
    m.all<-colMeans(rbind(X,X_u))
    
    
    matrixsqrt <- function(X) {
      decomposition<-svd(X)
      decomposition$u %*% diag(sqrt(decomposition$d)) %*% decomposition$v
    }
    
    sigma <- matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled)) %*% sigma %*% matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled))
    means <- matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled)) %*% (means-matrix(1,nrow(means),1) %*% m.labeled ) + matrix(1,nrow(means),1) %*% m.all 
    
    sigma<-lapply(1:ncol(Y),function(c){sigma})
    
  } else if (method=="ml") {
    
  }
  new("MCLinearDiscriminantClassifier", prior=prior, means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

# Constructor method: formula
# Removes intercept
MCLinearDiscriminantClassifier <- function(model, D, method="closedform", prior=NULL, scale=FALSE) {  
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
  
  # Fit model
  trained<-MCLinearDiscriminantClassifierXY(X, y, X_u, method=method,prior=prior,scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}