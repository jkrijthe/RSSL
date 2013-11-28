#' @include Classifier.R
setClass("NormalBasedClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="NormalBased Classifier"),
         contains="Classifier")

#' Show method for Normal Based Classifiers
#' 
#' Print some properties of classifiers based on classes modeled as Gaussians.
#' @usage show(object)
#'
#' @param object object of class NormalBasedClassifier
#'
#' @rdname show-methods
#' @aliases show,NormalBasedClassifier-method
setMethod("show", signature(object="NormalBasedClassifier"), function(object) {
  print(object@name)
  print(object@means)
})

#' Predict method for Normal Based Classifiers
#' 
#' Predict the class of new objects for a normal based classifier
#'
#' @usage predict(object, newdata)
#'
#' @param object object of class NormalBasedClassifier
#' @param newdata a matrix of data.frame, depending on what was used to train the classifier, with new objects to be classified
#' @return a vector with predicted classes
#' @rdname predict-methods
#' @aliases predict,NormalBasedClassifier-method
setMethod("predict", signature(object="NormalBasedClassifier"), function(object,newdata,prob=FALSE) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  M<-object@means

  G<-matrix(NA,nrow(X),length(object@sigma))
  for (c in 1:length(object@sigma)) {
    G[,c]<-matrix(1,nrow(X),1) %*% (t(log(object@prior[c,,drop=FALSE])) - 0.5 * diag(M[c,,drop=FALSE] %*% solve(object@sigma[[c]]) %*% t(M[c,,drop=FALSE]))) + X %*% solve(object@sigma[[c]]) %*% t(M[c,,drop=FALSE])
  }
  
  factor(apply(G, 1, which.max), labels=object@classnames,levels=1:length(object@classnames))
})

#' Loss method for Normal Based Classifiers
#' 
#' Return the MINUS log likelihood on the given dataset
#'
#' @usage loss(object, newdata, y)
#'
#' @param object object of class NormalBasedClassifier
#' @param newdata a matrix of data.frame, depending on what was used to train the classifier, with new objects to be classified
#' @param y if newdata is a matrix, y should contain the correct labels of the observations in newdata
#' @return The total minus log likelihood on the dataset
#' @rdname loss-methods
#' @aliases loss,NormalBasedClassifier-method 
setMethod("loss", signature(object="NormalBasedClassifier"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  if (is.null(y)) { stop("No labels supplied.")}
  Y <- model.matrix(~as.factor(y)-1)
  
  k<-ncol(X) # Number of features
  m<-object@means # Parameters of the NM classifier
  ll<-0
  for (c in 1:nrow(m)) {
    sigma<-object@sigma[[c]]
    
    Xc<-X[as.logical(Y[,c]), ,drop=FALSE] #Select all object in class c
    ll<-ll + nrow(Xc) * ( log(object@prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) #Add the constant part for each row in Xc
    ll<-ll + sum(-(1/2)*(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma) * (Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])) #Add the dynamic contribution
    # Note: much faster than: sum(-(1/2)*diag((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) 
  }
  
  return(-ll/nrow(X))
})
