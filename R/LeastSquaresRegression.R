setGeneric("loss",
           function(object, ...) standardGeneric("loss")
)

# Formal class definition
setClass("LeastSquaresRegression",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY"),
         prototype(name="LeastSquaresRegression",scaling=NULL))

LeastSquaresRegressionXY <- function(X, y, lambda=0, scale=FALSE, ...) {
  
  if (scale) {
    library(pls)
    scaling<-stdize(X[,2:ncol(X),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(X)]<-predict(scaling,X[,2:ncol(X),drop=FALSE])
    
  } else {scaling=NULL}
  
  n<-nrow(X)
  m<-ncol(X)
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  theta <- inv(t(X) %*% X + n*lambda*diag(c(0,rep(1,(m-1))))) %*% (t(X) %*% y)
  
  new("LeastSquaresRegression",
      scaling=scaling,
      theta=theta)
}

# Constructor
LeastSquaresClassifier <- function(modelform,D,lambda=0,scale=FALSE) {
  list2env(SSLDataFrameToMatrices(modelform,D,intercept=TRUE),environment())
  
  trained<-LeastSquaresRegressionXY(X, y, lambda, scale)
  trained@modelform<-modelform
  return(trained)
}

setMethod("loss", signature(object="LeastSquaresClassifier"), function(object, newdata, y=NULL) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    if (is.null(y)) { stop("No labels supplied.")}
    X<-newdata
  }
  #Scale the data
  if (!is.null(object@scaling)) { X<-predict(object@scaling,X) }
  
  return(sum((X %*% object@theta - y)^2))
})

setMethod("predict", signature(object="LeastSquaresClassifier"), function(object, newdata, probs=FALSE) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    X<-X
  } else {
    if (!is.matrix(newdata)) { browser(); stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  
  
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  
  return(expscore)
})

ICLeastSquaresRegressionXY<-function(X, y, X_u, lambda1=0, lambda2=0, scale=FALSE,lowerbound=-Inf, upperbound=Inf...,method="default") {
  
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { ginv(M) } #Another possibility: chol2inv(chol(M))
  
  if (nrow(X_u)==0) Xe<-X
  else Xe <- rbind(X,X_u)
  
  #Added
  if (scale) {
    library(pls)
    scaling<-stdize(Xe[,2:ncol(Xe),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(Xe)]<-predict(scaling,X[,2:ncol(Xe),drop=FALSE])
    X_u[,2:ncol(Xe)]<-predict(scaling,X_u[,2:ncol(Xe),drop=FALSE])
    
    Xe <- rbind(X,X_u)
  } else {scaling=NULL}
  
  
  m<-ncol(Xe)
  #browser()
  C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1))))) 
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
      if (lambda2>0) { return(mean((X %*% theta - y)^2) + lambda2 * sum(theta[2:nrow(theta),1]^2)) }
      else { return(mean((X %*% theta - y)^2)) }
    }
    
    O1 <- 2/nrow(X) * G %*% t(X) %*% y
    O2 <- 2/nrow(X) * G %*% t(X_u)
    O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
    
    if (lambda2>0) {
      O4<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% t(F) %*% y
      O5<- 2 * lambda2 * X_u %*% C %*% diag(c(0,rep(1,(m-1)))) %*% C %*% t(X_u)
    }
    
    opt_grad <- function(theta) {
      theta<-matrix(theta)
      if (lambda2>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta)
      else return(O1 + O2 %*% theta - O3)
    }
    
    theta <- rep(1.5,nrow(X_u))
    
    # Bounded optimization
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=lowerbound, upper=upperbound, control=list(fnscale=1))
    theta<-opt_result$par
    
    unlabels<-theta
  }
  
  theta <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1)))) ) %*% (t(Xe) %*% c(y,theta))
  
  new("LeastSquaresClassifier",
      classnames=1:length(unique(y)),
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      optimization=opt_result)
}

ICLeastSquaresClassifier<-function(modelform,D,lambda1=0,lambda2=0,hessian=FALSE,scale=FALSE,method="L-BFGS-B") {
  list2env(SSLDataFrameToMatrices(modelform,D,intercept=TRUE),environment())
  
  trained<-ICLeastSquaresClassifierXY(X,y,X_u, lambda1, lambda2, scale)
  trained@modelform<-modelform
  trained@classnames<-classnames
  return(trained)
}

setMethod("plot", signature(x="LeastSquaresClassifier",y="missing"), function(x) {
  object<-x
  #p<-qplot(object@D[is.na(object@D[,object@classname]),1],object@D[is.na(object@D[,object@classname]),2],color=object@unlabels)
  p<-qplot(object@D[,1],object@D[,2],color=object@D[,object@classname])
  p<-p+geom_abline(intercept = (1.5-x@theta[1])/x@theta[3], slope = -x@theta[2]/x@theta[3])
  return(p)
})