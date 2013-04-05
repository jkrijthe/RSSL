# Formal class definition
setClass("ClusterThenLabelClassifier",
         representation(theta="numeric"),
         prototype(name="Cluster then Label classifier"), 
         contains="SemiSupervisedClassifier")

ClusterThenLabelClassifier <- function(x,y,X_u,clustering,classifier) {
  if (is.formula(x) & is.data.frame(y)) {
    list2env(SSLDataFrameToMatrices(modelform,D,intercept=TRUE),environment())
    
    modelform<-modelform
    trained@classnames<-classnames
  }
  else if (is.matrix(x) & is.vector(y)) {
    
  }
  else {
    
  }

  #Clustering
  trained.clustering<-
  assignment.clustering<-

  K # Number of clusters
  
  classifiers<-list()
    
  trained.classifier.overal<-classifier(X,y)
  for (k in 1:K) {
    if (multiple classes)
      trained.classifiers[[k]]<-classifier(X[assignment.clustering==k,,drop=FALSE],y[assigment.clustering==k])
    else {
      trained.classifiers[[k]]<-trained.classifier.overal
    }
  }
  
  #Save trained clustering and corresponding classifiers.
  trained<-new("ClusterThenLabelClassifier",
      classnames=classnames,
      trained.clustering,
      trained.classifiers)
  if (is.formula(x)) {
    trained@modelform<-modelform
    trained@classnames<-classnames
  }
  
  return(trained)
}


  #Assign to cluster
  #Use classifier for cluster
setMethod("predict", signature(object="ClusterThenLabelClassifier"), function(object, newdata, y=NULL) {
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

setMethod("loss", signature(object="ClusterThenLabelClassifier"), function(object, newdata, y=NULL) {
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