# Formal Class Definition
setClass("NormalBasedClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="NormalBased Classifier"),
         contains="Classifier")

# Show method
setMethod("show", signature(object="NormalBasedClassifier"), function(object) {
  print(object@name)
  print(object@means)
})

# Predict method
setMethod("predict", signature(object="NormalBasedClassifier"), function(object,newdata) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=FALSE),environment())
    X<-X
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  M<-object@means

  G<-matrix(NA,nrow(X),length(object@sigma))
  for (c in 1:length(object@sigma)) {
    G[,c]<-matrix(1,nrow(X),1) %*% (t(log(object@prior[c,,drop=FALSE])) - 0.5 * diag(M[c,,drop=FALSE] %*% solve(object@sigma[[c]]) %*% t(M[c,,drop=FALSE]))) + X %*% solve(object@sigma[[c]]) %*% t(M[c,,drop=FALSE])
  }
  
  apply(G, 1, which.max)
})


# Loss: deterime the MINUS log likelihood
# 
setMethod("loss", signature(object="NormalBasedClassifier"), function(object, newdata, y=NULL) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=FALSE),environment())
    Y <- model.matrix(~as.factor(y)-1)
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    if (is.null(y)) { stop("No labels supplied.")}
    X<-newdata
    Y <- model.matrix(~as.factor(y)-1)
  }
  #Scale the data
  if (!is.null(object@scaling)) { X<-predict(object@scaling,X) }
  
  k<-ncol(X) # Number of features
  m<-object@means # Parameters of the NM classifier
  ll<-0
  for (c in 1:nrow(m)) {
    sigma<-object@sigma[[c]]
    
    Xc<-X[as.logical(Y[,c]), ,drop=FALSE] #Select all object in class c
    ll<-ll + nrow(Xc) * ( log(object@prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) #Add the constant part for each row in Xc
    ll<-ll+sum(-(1/2)*diag((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) #Add the dynamic contribution
  }
  
  return(-ll/nrow(X))
})