# Formal Class Definition
setClass("NearestMeanClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Nearest Mean Classifier"),
         contains="NormalBasedClassifier")

# Constructor method: XY
NearestMeanClassifierXY <- function(X, y, method="closedform",prior=NULL, scale=FALSE,  ...) {
  if (scale) {
    library(pls)
    scaling<-stdize(X, center = TRUE, scale = TRUE)
    X<-predict(scaling,X)
  } else {scaling=NULL}
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
    if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
    means<-t((t(X) %*% Y))/(colSums(Y))
    sigma<-mean((X-(Y %*% means))^2)
    sigma<-diag(ncol(X))*sigma
    sigma<-lapply(1:ncol(Y),function(c){sigma})
  } else if (method=="ml") {
    
    opt_func<-function(theta, X, y) {
      means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
      sigma<-theta[(ncol(Y)*ncol(X))+1]
      sigma<-diag(ncol(X))*sigma
      sigma<-lapply(1:ncol(Y),function(c){sigma})
      
      model<-new("NearestMeanClassifier", prior=prior, means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
    
      loss(model,X,y)
    }
    
    theta<-rep(0.01,3)
    opt_result <- optim(theta, opt_func, gr=NULL, X, y, method="L-BFGS-B", lower=c(-Inf,-Inf,0.000000001))
    theta<-opt_result$par
    
    means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
    sigma<-theta[(ncol(Y)*ncol(X))+1]
    sigma<-diag(ncol(X))*sigma
    sigma<-lapply(1:ncol(Y),function(c){sigma})
  }
  new("NearestMeanClassifier", prior=prior, means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

# Constructor method: formula
# Removes intercept
NearestMeanClassifier <- function(model, D, method="closedform", prior=NULL, scale=FALSE) {  
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
  
  # Fit model
  trained<-NearestMeanClassifierXY(X, y, method=method,prior=prior,scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}

# #OLD: S3 methods
# 
# nearestmean <- function(x, ...) UseMethod("nearestmean")
# 
# nearestmean.default<-function(modelform, D) {
#   classname<-all.vars(modelform)[1]
#   featurenames<-labels(terms(modelform,data=D))
#   classes<-unique(D[,all.vars(modelform)[1]])
#   m<-c()
#   for (i in classes) {
#     m<-rbind(m,sapply(D[D[classname]==i, featurenames, drop=FALSE],mean))
#   }
#   structure(list(m=m,classes=classes,featurenames=featurenames),class='nearestmean')
# }
# 
# predict.nearestmean<-function(object,newdata) {
#   if(!inherits(object, "nearestmean")) stop("Not a nearestmean object")
#   M<-object$m
#   knn(M,newdata[,object$featurenames,drop=FALSE],object$classes)
# }
# 
# logLik.nearestmean<-function(object,newdata) {
#   if(!inherits(object, "nearestmean")) stop("Not a nearestmean object")
#   k<-ncol(newdata)-1 # Number of features
#   m<-object$m # Parameters of the NM classifier
#   sigma<-diag(k) # NOTE: FIXED covariance matrix
#   ll<-0
#   for (c in 1:nrow(m)) {
#     X<-data.matrix(newdata[newdata$classes==object$classes[c],object$featurenames])
#     ll<-ll+nrow(X)*-(k/2)*log(2*pi)-(1/2)*log(det(sigma))
#     ll<-ll+sum(-(1/2)*(X-m[c,])%*%solve(sigma)%*%t(X-m[c,]))
#   }
#   return(ll)
# }