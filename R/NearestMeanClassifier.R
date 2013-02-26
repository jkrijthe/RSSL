# Formal Class Definition
setClass("NearestMeanClassifier",
         representation(means="matrix",covariance="matrix",sigma="numeric"),
         prototype(name="Nearest Mean Classifier"),
         contains="Classifier")

# Constructor method: XY
NearestMeanClassifierXY <- function(X, y, method="closedform",scale=TRUE, ...) {
  if (scale) {
    library(pls)
    scaling<-stdize(X, center = TRUE, scale = TRUE)
    X<-predict(scaling,X)
  } else {scaling=NULL}
  
  Y <- model.matrix(~y-1)
  
  if (method=="closedform") {
    means<-t((t(X) %*% Y))/(colSums(Y))
    sigma<-mean((X-(Y %*% means))^2)
  } else if (method=="ml") {
    
  }
  new("NearestMeanClassifier", means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

# Constructor method: formula
# Removes intercept
NearestMeanClassifier <- function(model, D, method="closedform",scale=TRUE) {  
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
  
  # Fit model
  trained<-NearestMeanClassifierXY(X, y, method=method,scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}

# Show method
setMethod("show", signature(object="NearestMeanClassifier"), function(object) {
  print(object@name)
  print(object@means)
})

# Predict method
setMethod("predict", signature(object="NearestMeanClassifier"), function(object,newdata) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=FALSE),environment())
    X<-X
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  M<-object@means
  knn(M, X, object@classnames)
})


# Loss: deterime the MINUS log likelihood
# 
setMethod("loss", signature(object="NearestMeanClassifier"), function(object,newdata,y=NULL) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=FALSE),environment())
    Y <- model.matrix(~y-1)
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    if (is.null(y)) { stop("No labels supplied.")}
    X<-newdata
    Y <- model.matrix(~y-1)
  }
  #Scale the data
  if (!is.null(object@scaling)) { X<-predict(object@scaling,X) }
  
  k<-ncol(X) # Number of features
  m<-object@means # Parameters of the NM classifier
  sigma<-diag(k)*object@sigma # Diagonal covariance matrix
  ll<-0
  for (c in 1:nrow(m)) {
    Xc<-X[as.logical(Y[,c]), ,drop=FALSE]
    ll<-ll+nrow(Xc)*-(k/2)*log(2*pi)-(1/2)*log(det(sigma))
    ll<-ll+sum(-(1/2)*(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))
  }
  return(-ll/nrow(X))
})

# logLik method: deterime the log likelihood
# 
setMethod("logLik", signature(object="NearestMeanClassifier"), function(object,newdata,...) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=FALSE),environment())
  } else {
    if (!is.matrix(X)) { stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  #Scale the data
  if (!is.null(object@scaling)) { X<-predict(object@scaling,X) }
  
  k<-ncol(X) # Number of features
  m<-object@means # Parameters of the NM classifier
  sigma<-diag(k)*object@sigma # Diagonal covariance matrix
  ll<-0
  for (c in 1:nrow(m)) {
    Xc<-X[as.logical(Y[,c]), ,drop=FALSE]
    ll<-ll+nrow(Xc)*-(k/2)*log(2*pi)-(1/2)*log(det(sigma))
    ll<-ll+sum(-(1/2)*(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))
  }
  return(ll)
})