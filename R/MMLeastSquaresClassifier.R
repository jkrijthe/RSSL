#' LogisticLossClassifier
#' @include LeastSquaresClassifier.R
setClass("MMLeastSquaresClassifier", 
         representation(theta="matrix",optimization="ANY",unlabels="ANY"), 
         prototype(name="MMLeastSquaresClassifier"), 
         contains="LeastSquaresClassifier")

#' Minimax LSC
#' @export
MMLeastSquaresClassifier<-function(X, y, X_u=NULL, lambda1=0, lambda2=0, intercept=TRUE,scale=FALSE,method="default", y_scale=FALSE, lambda3=0,trueprob=NULL,...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform

if (y_scale) {
    y_scaling<-mean(y)
    y<-y-y_scaling
  } else {
    y_scaling<-NULL
  }
  
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { ginv(M) } #Another possibility: chol2inv(chol(M))
  
  if (nrow(X_u)==0) Xe<-X
  else Xe <- rbind(X,X_u)
  
  if (!is.null(trueprob)) {
    mean_y<-trueprob
  } else {
    mean_y<-mean(y)
  }
  
  m<-ncol(Xe)

  C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1))))) 
  F<- X %*% C 
  G <- X_u %*% t(F) %*% F
  

  ## Quadratic Programming implementation
  if (method=="QP") {
    dvec <- X_u %*% C %*% t(X) %*% y
    Dmat <- G %*% t(X_u)
    Amat <- t(rbind(diag(nrow(X_u)), -diag(nrow(X_u))))
    
    # prior constraint
    
    
    bvec <- c(rep(1,nrow(X_u)), rep(-2,nrow(X_u)))
    #browser()
    #LowRankQP(Dmat,dvec,Amat,bvec,uvec,method="PFCF",verbose=FALSE,niter=200)@alpha
    #ipop(dvec, Dmat, l=0, u=0)
    theta<-solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)$solution
    
    #browser()
    #theta<-ipop(-dvec, Dmat, A=rbind(diag(nrow(X_u)),rep(1/nrow(X_u),nrow(X_u))), b=c(rep(1,nrow(X_u)),mean(y)), r=c(rep(1,nrow(X_u)),mean(y)), l=rep(1,nrow(X_u)), u=rep(2,nrow(X_u)))@primal
    unlabels<-theta
    opt_result<-0
    
  }
  ## Gradient Descent implementation
  else {
    opt_func <- function(theta) {
      theta<-matrix(theta)
      theta <- C %*% t(Xe) %*% rbind(matrix(y),theta)
      if (lambda2>0) { 
        if (intercept) { return(mean((X %*% theta - y)^2) + lambda2 * sum(theta[2:nrow(theta),1]^2))}
        else  { return(mean((X %*% theta - y)^2) + lambda2 * sum(theta^2)) + lambda3*(mean(theta)-mean_y)^2 }
      }
      else { return(mean((X %*% theta - y)^2)) + lambda3*(mean(theta)-mean_y)^2 }
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
      reg3<-lambda3*2*(rep(sum(theta),nrow(theta))-rep(nrow(theta)*mean_y,nrow(theta)))

      if (lambda2>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta + reg3)
      else return(O1 + O2 %*% theta - O3 + reg3  ) 
    }
    
    if (y_scale) {
    theta <- rep(1.5-y_scaling,nrow(X_u))
    
    # Bounded optimization
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0-y_scaling, upper=2.0-y_scaling, control=list(fnscale=1))
    } else{
      theta <- rep(1.5,nrow(X_u))
    
    # Bounded optimization
    opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1))
  
    }
    theta<-opt_result$par
    
    unlabels<-theta
  }
  
  theta <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(c(0,rep(1,(m-1)))) ) %*% (t(Xe) %*% c(y,theta))
  
  new("ICLeastSquaresClassifier",
      classnames=classnames,
      modelform=modelform,
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      intercept=intercept,
      optimization=opt_result,
      y_scaling=y_scaling)
}
