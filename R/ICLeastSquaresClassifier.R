ICLeastSquaresClassifierXY<-function(X, y, X_u, lambda1=0, lambda2=0, scale=FALSE,...,method="default") {
  
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
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1))
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

ExtraConstrainedICLeastSquaresClassifierXY<-function(X, y, X_u, lambda1=0, lambda2=0, scale=FALSE,...,method="default") {
  
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
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1))
    theta<-opt_result$par
    
    unlabels<-theta
  }
  
  # Project back to limited subspace
  #TODO
  
  theta <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1)))) ) %*% (t(Xe) %*% c(y,theta))
  
  new("LeastSquaresClassifier",
      classnames=1:length(unique(y)),
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      optimization=opt_result)
}