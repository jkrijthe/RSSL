library(quadprog)

#' @include Classifier.R
setClass("TSVM", 
         representation(),
         prototype(name="Transductive Support Vector Machine"), 
         contains="Classifier")

#' @title CCCP Transductive SVM classifier
#'
#' @param X matrix; Design matrix, intercept term is added within the function
#' @param y vector; Vector or factor with class assignments
#' @param X_u matrix; Design matrix of the unlabeled data, intercept term is added within the function
#' @param Clabeled numeric; Cost parameter of the SVM
#' @param Cunlabeled numeric; Cost parameter of the unlabeled objects
#' @param fracpos numeric; fraction of test points to assign to the first class
#' @param intercept TRUE if an intercept should be added to the model
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param ... additional arguments
#' @return S4 object of class TSVM with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' 
#' @export
TSVM_CCCPlin <- function(X, y, X_u, C, Cstar, s=-0.3, x_center=FALSE, scale=FALSE, eps=1e-6,...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  #y<-ModelVariables$y
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  
  y <- model.matrix(~y-1,data.frame(y))[,1]*2-1
  
  L <- nrow(X)
  U <- nrow(X_u)
  
  yu<-c(rep(-1,U),rep(1,U))
  Xe<-rbind(X,X_u,X_u)
  
  K <- X %*% t(X)
  sup <- solve_svm(K,y,C=C)
  w <- c((sup$alpha * y) %*% X, sup$b)
  w_prev <- rep(Inf,length(w))
  iterations <- 1 
  X<-cbind(X,1)
  X_u<-cbind(X_u,1)
  
  while (norm(matrix(w-w_prev))>eps) {
    
    hs <- function(x,s) {sapply(x,function(x,s){max(s-x,0)},s=s)}
    objective <- function(w, w_now) {
      w <- matrix(w,length(w))
      w_now <- matrix(w,length(w_now))
      (0.5 * (t(w) %*% w) + C * sum(hs(y *(X %*% w),s=1)) + Cstar * sum(hs(yu *(rbind(X_u,X_u) %*% w),s=1)) - colSums(-(yu *(rbind(X_u,X_u)))[hs(yu*(rbind(X_u,X_u) %*% w_now), s=0)>0,1:3]) %*% w)
    }
    w_prev<-w
    print(0.5 * (t(w) %*% w) + C * sum(hs(y *(X %*% w),s=1)) + Cstar * sum(hs(yu *rbind(X_u,X_u) %*% w,s=1)) - sum(hs(yu*rbind(X_u,X_u) %*% w, s=0)))
    w<-optim(w, objective, w_now=w)$par
    print(w)
    print(0.5 * (t(w) %*% w) + C * sum(hs(y *(X %*% w),s=1)) + Cstar * sum(hs(yu *rbind(X_u,X_u) %*% w,s=1)) - sum(hs(yu*rbind(X_u,X_u) %*% w, s=0)))
    iterations <- iterations + 1
  }
  b<-w[length(w)]
  return(list(w=w[-length(w)],b=b))
}

# SVM solve.QP implementation
solve_svm <- function(K, y, C=1) {
  n <- nrow(K)
  
  Dmat <- t(y*K)*y
  Dmat <- Dmat + 0.0000001*diag(n)
  dvec <- c(rep(1,n))
  
  Amat<-rbind(y,
              diag(n),
              -diag(n)           
  )
  
  bvec<-c(0,rep(0,n),rep(-C,n))
  
  solution<-solve.QP(Dmat = Dmat,dvec = dvec,Amat = t(Amat),bvec = bvec,meq=1)
  print(solution$value)
  alpha<-solution$solution
  w <- alpha %*% (y*K)
  print(y[alpha>0.0001 & alpha<C]*(1-(w*y)[alpha>0.0001 & alpha<C]))
  bias <- median(y[alpha>0.0001 & alpha<C]*(1-(w*y)[alpha>0.0001 & alpha<C]))
  
  object<-list(b=bias,alpha=solution$solution)
  class(object)<-"SVM"
  return(object)
}

