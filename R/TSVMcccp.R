#' @include Classifier.R
setClass("TSVMcccp", 
         slots=c(alpha="ANY",b="ANY",K="ANY", beta="ANY"),
         prototype=list(name="Transductive Support Vector Machine training using CCCP"), 
         contains="Classifier")

#' Transductive SVM classifier using the convex concave procedure
#'
#' @param X matrix; Design matrix, intercept term is added within the function
#' @param y vector; Vector or factor with class assignments
#' @param X_u matrix; Design matrix of the unlabeled data, intercept term is added within the function
#' @param C numeric; Cost parameter of the SVM
#' @param Cstar numeric; Cost parameter of the unlabeled objects
#' @param s numeric; parameter controlling the loss function of the unlabeled objects
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param verbose logical; print debugging messages (default: FALSE)
#' @param ... additional arguments
#' @inheritParams BaseClassifier
#' @return S4 object of class TSVM with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' 
#' @export
TSVMcccp <- function(X, y, X_u, C, Cstar, s=-0.3, x_center=FALSE, scale=FALSE, eps=1e-9,verbose=FALSE, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  Y <- ModelVariables$Y
  
  y <- as.numeric(Y*2-1)
  
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
  
  if (verbose) cat("Loss  @ Start: ",loss(w),'\n')
  
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
    b <- median(vals)
    if (verbose) cat("Possible Bias values: ", vals, "\n")
    if (verbose) w <- c((alpha[-length(alpha)]) %*% Xe, b)
    if (verbose) cat("Loss @ iteration ",iterations,": ",loss(w),'\n')
    #if (verbose) cat("w    @ iteration ",iterations,": ",w,'\n')
    
    # Update Beta
    f_pred <- (alpha) %*% Ke + b
    beta_prev <- beta
    beta <- rep(0,2*U)
    beta[(y_used*f_pred)[(L+1):(L+2*U)]<s] <- Cstar
    
    iterations <- iterations + 1
  }
  return(new(Class = "TSVMcccp",
             alpha=alpha,
             b=b,
             K=Ke,
             beta=beta))
}

#' @rdname rssl-predict
#' @aliases predict,TSVMcccp-method
setMethod("predict", signature(object="TSVMcccp"), function(object, newdata, probs=FALSE) {
  
})

#' SVM solve.QP implementation
#' @param K Kernel matrix
#' @param y Output vector
#' @param C Cost parameter
#' @export
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
  if (FALSE) cat(solution$value,"\n")
  alpha<-solution$solution
  w <- alpha %*% (y*K)
  if (FALSE)  cat(y[alpha>0.0001 & alpha<C]*(1-(w*y)[alpha>0.0001 & alpha<C]),"\n")
  bias <- median(y[alpha>0.0001 & alpha<C]*(1-(w*y)[alpha>0.0001 & alpha<C]))
  
  object<-list(b=bias,alpha=solution$solution)
  class(object)<-"SVM"
  return(object)
}

#' @title Linear CCCP Transductive SVM classifier
#'
#' @description This method is mostly for debugging purposes, since its updates are done using numerical gradient calculations.
#'
#' @param X matrix; Design matrix, intercept term is added within the function
#' @param y vector; Vector or factor with class assignments
#' @param X_u matrix; Design matrix of the unlabeled data, intercept term is added within the function
#' @param C numeric; Cost parameter of the SVM
#' @param Cstar numeric; Cost parameter of the unlabeled objects
#' @param s numeric; parameter controlling the loss function of the unlabeled objects
#' @param scale logical; If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param verbose logical; print debugging messages (default: FALSE)
#' @param eps numeric; Convergence criterion
#' @param init numeric; Initial classifier parameters to start the convex concave procedure
#' @param ... additional arguments
#' @inheritParams BaseClassifier
#' @return S4 object of class TSVM with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' 
#' @export
TSVMcccp_lin <- function(X, y, X_u, C, Cstar, s=-0.3, x_center=FALSE, scale=FALSE, eps=1e-6, verbose=FALSE, init=NULL, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  #y<-ModelVariables$y
  Y <- ModelVariables$Y
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  
  y <- as.numeric(Y*2-1)

  
  L <- nrow(X)
  U <- nrow(X_u)
  
  yu<-c(rep(-1,U),rep(1,U))
  Xe<-rbind(X,X_u,X_u)
  
  K <- X %*% t(X)
  sup <- solve_svm(K,y,C=C)
  if (is.null(init)) {
    w <- c((sup$alpha * y) %*% X, sup$b)
  } else {
    w <- init
  }
  w_prev <- rep(Inf,length(w))
  iterations <- 1 
  X<-cbind(X,1)
  X_u<-cbind(X_u,1)
  
  hs <- function(x,s) {sapply(x,function(x,s){max(s-x,0)},s=s)} # Adaptable Hinge Loss
  loss <- function(w) { 0.5 * (t(w) %*% w) + C * sum(hs(y *(X %*% w),s=1)) + Cstar * sum(hs(yu *rbind(X_u,X_u) %*% w,s=1)) - sum(hs(yu*rbind(X_u,X_u) %*% w, s=0))}
  
  if (verbose) cat("Loss @ Start   :", loss(w),"\n")
  
  while (norm(matrix(w-w_prev))>eps) {
    objective <- function(w, w_now) {
      w <- matrix(w,length(w))
      w_now <- matrix(w,length(w_now))
      (0.5 * (t(w[-length(w)]) %*% w[-length(w)]) + C * sum(hs(y *(X %*% w),s=1)) + Cstar * sum(hs(yu *(rbind(X_u,X_u) %*% w),s=1)) - colSums(-(yu *(rbind(X_u,X_u)))[hs(yu*(rbind(X_u,X_u) %*% w_now), s=0)>0,1:3]) %*% w)
    }

    w_prev<-w
    w<-optim(w, objective, w_now=w)$par
    if (verbose) cat("Loss @ iter ",iterations,": ", loss(w),"\n")
    iterations <- iterations + 1
  }
  b<-w[length(w)]
  
  return(new("LinearSVM",
             w=c(b,w),
             scaling=scaling,
             modelform=NULL,
             classnames=classnames))
}
