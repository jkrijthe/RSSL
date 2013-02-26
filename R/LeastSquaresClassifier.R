setGeneric("loss",
           function(object, ...) standardGeneric("loss")
)

# Formal class definition
setClass("LeastSquaresClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY"),
         prototype(name="FisherClassifier",scaling=NULL), 
         contains="Classifier")

LeastSquaresClassifierXY <- function(modelform, D, lambda=0, scale=FALSE) {
  
  if (scale) {
    library(pls)
    scaling<-stdize(X[,2:ncol(X),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(X)]<-predict(scaling,X[,2:ncol(X),drop=FALSE])
    
  } else {scaling=NULL}
  
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  theta <- inv(t(X) %*% X + n*lambda*diag(m)) %*% (t(X) %*% y)
  
  new("FisherClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta)
}

# Constructor
LeastSquaresClassifier<-function(modelform,D,lambda=0,scale=FALSE) {
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
  
  trained<-LeastSquaresClassifierXY(modelform, D, lambda, scale)
  trained@modelform<-modelform
  trained@classnames<-classnames
}

predictxy<- function(object,X) {
  probs=FALSE
  
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore>1.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  if (probs)
  {
    return(expscore)
  } else return(classes)
}

setMethod("loss", signature(object="FisherClassifier"), function(object, D) {
  X<-model.matrix(object@modelform, D)
  y<-data.matrix(as.integer(D[,object@classname]))
  return(sum((X %*% object@theta - y)^2))
})

setMethod("predict", signature(object="FisherClassifier"), function(object, D, probs=FALSE) {
  
  D[,object@classname] <- 1
  X<-model.matrix(modelform,D)
  
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore>1.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  if (probs)
  {
    return(expscore)
  } else return(classes)
})

ICLeastSquaresClassifierXY<-function() {
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { ginv(M) } #Another possibility: chol2inv(chol(M))
  
  Xe <- rbind(X,X_u)
  
  #Added
  if (scale) {
    library(pls)
    scaling<-stdize(Xe[,2:ncol(Xe),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(Xe)]<-predict(scaling,X[,2:ncol(Xe),drop=FALSE])
    X_u[,2:ncol(Xe)]<-predict(scaling,X_u[,2:ncol(Xe),drop=FALSE])
    
    Xe <- rbind(X,X_u)
  } else {scaling=NULL}
  
  C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(m)) 
  F<- X %*% C 
  G <- X_u %*% t(F) %*% F
  
  if (method=="QP") {
    dvec <- X_u %*% C %*% t(X) %*% y
    Dmat <- G %*% t(X_u)
    Amat <- t(rbind(diag(nrow(X_u)), -diag(nrow(X_u))))
    
    # prior constraint
    
    
    bvec <- c(rep(1,nrow(X_u)), rep(-2,nrow(X_u)))
    #browser()
    #LowRankQP(Dmat,dvec,Amat,bvec,uvec,method="PFCF",verbose=FALSE,niter=200)@alpha
    #ipop(dvec, Dmat, l=0, u=0)
    #theta<-solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)$solution
    
    #browser()
    theta<-ipop(-dvec, Dmat, A=rbind(diag(nrow(X_u)),rep(1/nrow(X_u),nrow(X_u))), b=c(rep(1,nrow(X_u)),mean(y)), r=c(rep(1,nrow(X_u)),mean(y)), l=rep(1,nrow(X_u)), u=rep(2,nrow(X_u)))@primal
    unlabels<-theta
    opt_result<-0
    
  }
  else {
    
    
    
    opt_func <- function(theta) {
      theta<-matrix(theta)
      theta <- C %*% t(Xe) %*% rbind(matrix(y),theta)
      if (lambda>0) { return(mean((X %*% theta - y)^2) + lambda * sum(theta^2)) }
      else { return(mean((X %*% theta - y)^2)) }
    }
    
    O1 <- 2/nrow(X) * G %*% t(X) %*% y
    O2 <- 2/nrow(X) * G %*% t(X_u)
    O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
    
    if (lambda>0) {
      O4<- 2 * lambda * X_u %*% C%*% t(F) %*% y
      O5<- 2 * lambda * X_u %*% C %*% C %*% t(X_u)
    }
    
    opt_grad <- function(theta) {
      theta<-matrix(theta)
      if (lambda>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta)
      else return(O1 + O2 %*% theta - O3)
    }
    
    theta <- rep(1.5,nrow(X_u))
    
    # Bounded optimization
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1),hessian=hessian)
    theta<-opt_result$par
    
    unlabels<-theta
  }
  X<-rbind(X,X_u)
  theta <- inv(t(X) %*% X) %*% (t(X) %*% c(y,theta))
  
  new("LeastSquaresClassifier",
      modelform=modelform,
      classname=classname,
      classnames=classnames,
      D=D,
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      optimization=opt_result)
}

SemiSupervisedFisherClassifier<-function(modelform,D,lambda=0,lambda1=0,hessian=FALSE,scale=FALSE,method="L-BFGS-B") {
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),environment())
  
  trained<-ICLeastSquaresClassifierXY(modelform, D, lambda, scale)
  trained@modelform<-modelform
  trained@classnames<-classnames
}

setMethod("plot", signature(x="LeastSquaresClassifier",y="missing"), function(x) {
  object<-x
  #p<-qplot(object@D[is.na(object@D[,object@classname]),1],object@D[is.na(object@D[,object@classname]),2],color=object@unlabels)
  p<-qplot(object@D[,1],object@D[,2],color=object@D[,object@classname])
  p<-p+geom_abline(intercept = (1.5-x@theta[1])/x@theta[3], slope = -x@theta[2]/x@theta[3])
  return(p)
})