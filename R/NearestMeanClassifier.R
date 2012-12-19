setClass("Classifier",
         representation(D = "data.frame",name="character",modelform="formula",classname="character",classnames="factor",featurenames="character")
         )
setClass("SemiSupervisedClassifier",contains="Classifier")

# Formal Class Definition
setClass("NearestMeanClassifier",
         representation(means="matrix",covariance="matrix"),
         prototype(name="Nearest Mean Classifier"),
         contains="Classifier")

# Constructor method: formula
NearestMeanClassifier <- function(modelform, D) {
  classname<-all.vars(modelform)[1]
  featurenames<-labels(terms(modelform,data=D))
  classes<-unique(D[,all.vars(modelform)[1]])
  m<-c()
  for (i in classes) {
    m<-rbind(m,sapply(D[D[classname]==i, featurenames, drop=FALSE],mean))
  }
  new("NearestMeanClassifier", D=D, means=m,classname=classname,classnames=classes,featurenames=featurenames,modelform=modelform)
}
#setMethod("NearestMeanClassifier",signature(modelform="formula"), NearestMeanClassifier)

# Show method
setMethod("show", signature(object="NearestMeanClassifier"), function(object) {
  cat(object@name)
  print(object@means)
})

# Predict method
setMethod("predict", signature(object="NearestMeanClassifier"), function(object,newdata) {
  M<-object@means
  knn(M,newdata[,object@featurenames,drop=FALSE],object@classnames)
})

# logLik method: deterime the log likelihood
setMethod("logLik", signature(object="NearestMeanClassifier"), function(object,newdata) {
  k<-ncol(newdata)-1 # Number of features
  m<-object@means # Parameters of the NM classifier
  sigma<-diag(k) # NOTE: FIXED covariance matrix
  ll<-0
  for (c in 1:nrow(m)) {
    X<-data.matrix(newdata[newdata$classes==object@classnames[c],object@featurenames])
    ll<-ll+nrow(X)*-(k/2)*log(2*pi)-(1/2)*log(det(sigma))
    ll<-ll+sum(-(1/2)*(X-m[c,])%*%solve(sigma)%*%t(X-m[c,]))
  }
  return(ll)
})