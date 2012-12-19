#OLD: S3 methods

nearestmean <- function(x, ...) UseMethod("nearestmean")

nearestmean.default<-function(modelform, D) {
  classname<-all.vars(modelform)[1]
  featurenames<-labels(terms(modelform,data=D))
  classes<-unique(D[,all.vars(modelform)[1]])
  m<-c()
  for (i in classes) {
    m<-rbind(m,sapply(D[D[classname]==i, featurenames, drop=FALSE],mean))
  }
  structure(list(m=m,classes=classes,featurenames=featurenames),class='nearestmean')
}

predict.nearestmean<-function(object,newdata) {
  if(!inherits(object, "nearestmean")) stop("Not a nearestmean object")
  M<-object$m
  knn(M,newdata[,object$featurenames,drop=FALSE],object$classes)
}

logLik.nearestmean<-function(object,newdata) {
  if(!inherits(object, "nearestmean")) stop("Not a nearestmean object")
  k<-ncol(newdata)-1 # Number of features
  m<-object$m # Parameters of the NM classifier
  sigma<-diag(k) # NOTE: FIXED covariance matrix
  ll<-0
  for (c in 1:nrow(m)) {
    X<-data.matrix(newdata[newdata$classes==object$classes[c],object$featurenames])
    ll<-ll+nrow(X)*-(k/2)*log(2*pi)-(1/2)*log(det(sigma))
    ll<-ll+sum(-(1/2)*(X-m[c,])%*%solve(sigma)%*%t(X-m[c,]))
  }
  return(ll)
}