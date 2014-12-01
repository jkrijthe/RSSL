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
TSVM_CCCP <- function(X, y, X_u, C, Cstar, s=-0.3, x_center=FALSE, scale=FALSE, eps=1e-6,verbose=TRUE,...) {
  
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
  
  Xe<-rbind(X,X_u,X_u)

  K <- X %*% t(X)
  Ke <- Xe %*% t(Xe)
  
  # Calculate column for balancing constraint
  constraintcol <- rowMeans(Ke[,(L+1):(L+U),drop=FALSE])
  
  Ke <- rbind(cbind(Ke,constraintcol),c(constraintcol,mean(Ke[(L+1):(L+U),(L+1):(L+U)])))
  
  ## The Main CCCP algorithm
  sup <- solve_svm(K,y,C=C)
  f_pred <- (sup$alpha * y) %*% (Ke[1:L,])
  
  ysup<-y
  loss <- function(w) {
    hs <- function(x,s) {sapply(x,function(x,s){max(s-x,0)},s=s)}
    X<-cbind(X,1)
    X_u<-cbind(X_u,1)
    yu<-c(rep(-1,U), rep(1,U))
    y<-ysup
    0.5 * (t(w) %*% w) + C * sum(hs(y *(X %*% w),s=1)) + Cstar * sum(hs(yu *rbind(X_u,X_u) %*% w,s=1)) - sum(hs(yu*rbind(X_u,X_u) %*% w, s=0))
  }
  w <- c((sup$alpha * y) %*% X, sup$b)
  
  if (verbose) cat("Loss begin: ",loss(w),'\n')
  
  y_used <- c(y, rep(-1,U), rep(1,U),1) # The labeling that will be used, unlabeled objects are added twice, with both labels
  N <- L+2*U
  beta <- rep(0,2*U)
  beta[(y_used*f_pred)[(L+1):(L+2*U)]<s] <- Cstar
  xi <- y_used # Include the balancing constraint
  xi[length(xi)] <- mean(y)
  y <- y_used
  
  beta_prev<-rep(Inf,length(beta))
  iterations <- 1 
  while (norm(matrix(beta-beta_prev))>eps) {
    
    #Define the constraints
    Amat <- cbind(rep(1,N+1),
                  rbind(diag(y[1:L]),matrix(0,N-L+1,L)),
                  rbind(diag(-y[1:L]),matrix(0,N-L+1,L)),
                  rbind(matrix(0,L,N-L),diag(y[(L+1):N]),rep(0,N-L)),
                  rbind(matrix(0,L,N-L),diag(-y[(L+1):N]),rep(0,N-L))
                  ) 
    bvec <- c(0,
              rep(0,L),
              rep(-C,L),
              -beta,
              beta-Cstar)
    
    #Solve for alpha
    res <- solve.QP(Dmat = (Ke+diag(0.00000001,nrow(Ke))), dvec = xi, Amat=Amat, bvec=bvec,meq=1)
    alpha<-res$solution
    
    
    
    eps <- 0.01
    #Compute the bias term
    w <- (alpha) %*% Ke
    
    vals<-c(
      y[c(1:L,N+1)][(y*alpha)[c(1:L)]>eps & (y*alpha)[c(1:L)]<(C-eps)]*(1-(w*y)[c(1:L,N+1)][(y*alpha)[c(1:L)]>eps & (y*alpha)[c(1:L)]<(C-eps)]),
      y[(L+1):(N)][(y*alpha)[(L+1):(N)]>-beta+eps & (y*alpha)[(L+1):(N)]<Cstar-eps-beta]*(1-(w*y)[(L+1):(N)][(y*alpha)[(L+1):(N)]>-beta+eps & (y*alpha)[(L+1):(N)]<Cstar-eps-beta])
    )
    b<-median(vals)
    print(vals)
    #w <- c((alpha) %*% Xe, b)
    #if (verbose) cat("Loss @ iteration ",iterations,": ",loss(w),'\n')
    #if (verbose) cat("w    @ iteration ",iterations,": ",w,'\n')
    
    # Update Beta
    f_pred <- (alpha) %*% Ke + b
    beta_prev <- beta
    beta <- rep(0,2*U)
    beta[(y_used*f_pred)[(L+1):(L+2*U)]<s] <- Cstar
    
    iterations <- iterations + 1
  }
  return(list(alpha=alpha,b=b,K=Ke,beta=beta))
}

#' SVM solve.QP implementation
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
