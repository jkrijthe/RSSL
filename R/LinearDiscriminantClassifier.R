# Formal Class Definition
setClass("LinearDiscriminantClassifier",
         representation(means="matrix",covariance="matrix",sigma="numeric"),
         prototype(name="Nearest Mean Classifier"),
         contains="Classifier")

# Constructor method: XY
LinearDiscriminantClassifierXY <- function(X, Y, method="closedform",scale=TRUE) {
  if (scale) {
    library(pls)
    scaling<-stdize(X, center = TRUE, scale = TRUE)
    X<-predict(scaling,X)
  } else {scaling=NULL}
  
  if (method=="closedform") {
    means<-t((t(X) %*% Y))/(colSums(Y))
    sigma<-mean((X-(Y %*% means))^2)
    
  } else if (method=="ml") {
    
  }
  new("NearestMeanClassifier", means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

# Constructor method: formula
# Removes intercept
LinearDiscriminantClassifier <- function(model, D, method="closedform",scale=TRUE) {  
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
  
  # Fit model
  trained<-NearestMeanClassifierXY(X, Y, method=method,scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}

# Show method
setMethod("show", signature(object="LinearDiscriminantClassifier"), function(object) {
  print(object@name)
  print(object@means)
})

# Predict method
setMethod("predict", signature(object="LinearDiscriminantClassifier"), function(object,newdata) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
    X<-X_u
  } else {
    if (!is.matrix(X)) { stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  
  M<-object@means
  knn(M, X, object@classnames)
})

# logLik method: deterime the log likelihood
# 
setMethod("logLik", signature(object="LinearDiscriminantClassifier"), function(object,newdata) {
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
    Xc<-X[Y[,c],]
    ll<-ll+nrow(Xc)*-(k/2)*log(2*pi)-(1/2)*log(det(sigma))
    ll<-ll+sum(-(1/2)*(Xc-m[c,])%*%solve(sigma)%*%t(Xc-m[c,]))
  }
  return(ll)
})