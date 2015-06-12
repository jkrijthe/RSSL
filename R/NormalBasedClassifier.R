#' @include Classifier.R
setClass("NormalBasedClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="NormalBased Classifier"),
         contains="Classifier")



#' @rdname rssl-formatting
#' @aliases show,NormalBasedClassifier-method
setMethod("show", signature(object="NormalBasedClassifier"), function(object) {
  print(object@name)
  print(object@means)
})

#' @rdname rssl-predict
#' @aliases predict,NormalBasedClassifier-method
setMethod("predict", signature(object="NormalBasedClassifier"), function(object,newdata,probs=FALSE) {
  
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  M<-object@means

  G<-matrix(NA,nrow(X),length(object@sigma))
  for (c in 1:length(object@sigma)) {
    G[,c]<-matrix(1,nrow(X),1) %*% (t(log(object@prior[c,,drop=FALSE])) - 0.5 * diag(M[c,,drop=FALSE] %*% solve(object@sigma[[c]]) %*% t(M[c,,drop=FALSE]))) + X %*% solve(object@sigma[[c]]) %*% t(M[c,,drop=FALSE])
  }
  
  
  factor(apply(G, 1, which.max), labels=object@classnames,levels=1:length(object@classnames))
})

#' Return the MINUS log likelihood on the given dataset
#' @rdname loss-methods
#' @aliases loss,NormalBasedClassifier-method 
setMethod("loss", signature(object="NormalBasedClassifier"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,object@scaling,intercept=FALSE,classnames=object@classnames)
  X<-ModelVariables$X
  y<-ModelVariables$y
  if (is.null(y)) { stop("No labels supplied.")}
  Y <- model.matrix(~as.factor(y)-1)
  
  k<-ncol(X) # Number of features
  m<-object@means # Parameters of the NM classifier
  ll<-0
  losses <- rep(NA,nrow(X))
  for (c in 1:nrow(m)) {
    sigma<-object@sigma[[c]]
    
    Xc<-X[as.logical(Y[,c]), ,drop=FALSE] #Select all objects in class c
    ll<- ll + nrow(Xc) * ( log(object@prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) #Add the constant part for each row in Xc
    ll<- ll + -(1/2)*sum((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma) * (Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])) #Add the dynamic contribution
    # Note: much faster than: sum(-(1/2)*diag((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) 
    losses[as.logical(Y[,c])] <- ( log(object@prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) + -(1/2)*rowSums((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma) * (Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))
    
  }
#   if (-ll<0) {browser()}
  #print(-sum(losses))
  return(-losses)
})

#' @param newdata design matrix
#' @param Y class responsibility matrix
#' @rdname losspart-methods
#' @aliases losspart,NormalBasedClassifier-method
setMethod("losspart", signature(object="NormalBasedClassifier"), function(object, newdata, Y) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  
  k<-ncol(X) # Number of features
  m<-object@means # Parameters of the NM classifier
  ll<-0
  for (c in 1:nrow(m)) {
    sigma<-object@sigma[[c]]
    Xc<-X
    ll<-ll + sum(Y[,c]) * ( log(object@prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) #Add the constant part for each row in Xc
    ll<-ll + sum(t(Y[,c,drop=FALSE]) %*% (-(1/2)*(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma) * (Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) #Add the dynamic contribution
    # Note: much faster than: sum(-(1/2)*diag((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) 
  }
  
  return(-ll)
})

#' @param newdata Design matrix of labeled objects
#' @param Y label matrix of labeled objects
#' @param X_u Design matrix of unlabeled objects
#' @param Y_u label matrix of unlabeled objects
#' @rdname losslogsum-methods
#' @aliases losslogsum,NormalBasedClassifier-method
setMethod("losslogsum", signature(object="NormalBasedClassifier"), function(object, newdata, Y, X_u, Y_u) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  k<-ncol(X) # Number of features
  m<-object@means # Parameters of the NM classifier
  ll<-0
  sigma<-object@sigma[[1]]
  for (c in 1:nrow(m)) {
    sigma<-object@sigma[[c]]
    
    Xc<-X
    ll<-ll + sum(Y[,c]) * ( log(object@prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) #Add the constant part for each row in Xc
    ll<-ll + sum(t(Y[,c,drop=FALSE]) %*% (-(1/2)*(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma) * (Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) #Add the dynamic contribution
    # Note: much faster than: sum(-(1/2)*diag((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) 
  }  
  
  
    Xc<-X_u
    for (i in 1:nrow(Y_u)) {
      ll <- ll + log(exp(( log(object@prior[1,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) + ( sum(-(1/2)*(Xc[i,]-matrix(1,1,1) %*% m[1, ,drop=FALSE])%*%solve(sigma) * (Xc[i,]-matrix(1,1,1) %*% m[1, ,drop=FALSE])))) + exp(( log(object@prior[2,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) + ( sum(-(1/2)*(Xc[i,]-matrix(1,1,1) %*% m[2, ,drop=FALSE])%*%solve(sigma) * (Xc[i,]-matrix(1,1,1) %*% m[2, ,drop=FALSE]))))) #Add the constant part for each row in Xc
    }
    # Note: much faster than: sum(-(1/2)*diag((Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma)%*%t(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) 
  
  
  return(-ll)
})

#' @param newdata matrix of dataframe of objects to be classified
#' @rdname posterior-methods
#' @aliases posterior,NormalBasedClassifier-method
setMethod("posterior", signature(object="NormalBasedClassifier"), function(object,newdata) {
  
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  M<-object@means
  
  G<-matrix(NA,nrow(X),length(object@sigma))
  for (c in 1:length(object@sigma)) {
    S <- object@sigma[[c]] # Covariance matrix
    k <- ncol(S) # Dimensionality
    G[,c]<-log(object@prior[c,,drop=FALSE]) - (k/2) * log(2*pi)- 0.5*log(det(S)) - rowSums( ((X-matrix(1,nrow(X),1) %*% M[c,,drop=FALSE]) %*% solve(S)) * (X-matrix(1,nrow(X),1) %*% M[c,,drop=FALSE])) 
  }
  
  posteriors <- G-(log(rowSums(exp(G-apply(G,1,max))))+apply(G,1,max)) # More stable way of doing logsumexp
  posteriors <- exp(posteriors)
  colnames(posteriors)<-object@classnames
  return(posteriors)
})

#' @rdname boundaryplot-methods
#' @aliases boundaryplot,LeastSquaresClassifier-method
#' @export
setMethod("boundaryplot", signature(object="NormalBasedClassifier"), function(object, p,...) {
#   w <- solve(object@sigma[[1]]) %*% t(object@means[1,, drop=FALSE]-object@means[2, drop=FALSE])
#   w0<- -(log(object@prior[2]/object@prior[1])+0.5*(object@means[1,, drop=FALSE]+object@means[2, drop=FALSE])%*%w)
#   
#   xd<-c(0,-0.33)
#   xd<-c(-1,-1.35)
  cat("Note: Boundary plot currently does not take into account data scaling!\n")
  w<- -(object@means[2,, drop=FALSE]-object@means[1,, drop=FALSE]) %*% solve(object@sigma[[1]])
  
  w0<-(log(object@prior[1])-log(object@prior[2])- 0.5*object@means[1,, drop=FALSE] %*% solve(object@sigma[[1]]) %*% t(object@means[1,, drop=FALSE]) + 0.5*object@means[2,, drop=FALSE] %*% solve(object@sigma[[1]]) %*% t(object@means[2,, drop=FALSE]))
  
#   log(object@prior[1])-0.5*object@means[1,, drop=FALSE] %*% solve(object@sigma[[1]]) %*% t(object@means[1,, drop=FALSE]) + object@means[1,, drop=FALSE] %*% solve(object@sigma[[1]]) %*% xd
#   log(object@prior[2])-0.5*object@means[2,, drop=FALSE] %*% solve(object@sigma[[1]]) %*% t(object@means[2,, drop=FALSE]) + object@means[2,, drop=FALSE] %*% solve(object@sigma[[1]]) %*% xd
  
#   w0+w%*%c(-1,-1.35)
#   abline(-(log(object@prior[1]/object@prior[2])-0.5*(object@means[1,, drop=FALSE]+object@means[2, drop=FALSE])%*%w)/w[2],-w[1]/w[2])
  p<-p+scale_linetype_manual(aes(z),name="Classifier", values=c("C1"=2, "C2"="twodash"))
#   p<-p+geom_abline(aes(="C2"),intercept = (log(object@prior[1]/object@prior[2])-0.5*(object@means[1,, drop=FALSE]+object@means[2, drop=FALSE])%*%w)/w[2], slope = -w[1]/w[2],linetype=1,...)



  p+geom_abline(aes(z="C1"),intercept = -w0/w[2], slope = -w[1]/w[2],linetype=1,...)
  
  })
