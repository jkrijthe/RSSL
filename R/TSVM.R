#' @include Classifier.R
setClass("TSVM", 
         slots=c(alpha="ANY",beta="ANY",labeled="integer",balancing_constraint="logical",iterations="numeric"),
         prototype=list(name="Transductive Support Vector Machine"), 
         contains="SVM")

#' Transductive SVM classifier using the convex concave procedure
#'
#' Transductive SVM using the CCCP algorithm as proposed by Collobert et al. (2006) implemented in R using the quadprog package. The implementation does not handle large datasets very well, but can be useful for smaller datasets and visualization purposes.
#' 
#' C is the cost associated with labeled objects, while Cstar is the cost for the unlabeled objects. s control the loss function used for the unlabeled objects: it controls the size of the plateau for the symmetric ramp loss function. The balancing constraint makes sure the label assignments of the unlabeled objects are similar to the prior on the classes that was observed on the labeled data.
#'
#' @param C numeric; Cost parameter of the SVM
#' @param Cstar numeric; Cost parameter of the unlabeled objects
#' @param s numeric; parameter controlling the loss function of the unlabeled objects (generally values between -1 and 0)
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param verbose logical; print debugging messages, only works for vanilladot() kernel (default: FALSE)
#' @param balancing_constraint logical; Whether a balancing constraint should be enforced that causes the fraction of objects assigned to each label in the unlabeled data to be similar to the label fraction in the labeled data.
#' @param max_iter integer; Maximum number of iterations
#' @inheritParams BaseClassifier
#' @family RSSL classifiers
#' @example inst/examples/example-TSVM.R
#' @references Collobert, R. et al., 2006. Large scale transductive SVMs. Journal of Machine Learning Research, 7, pp.1687-1712.
#' 
#' @export
TSVM <- function(X, y, X_u, C, Cstar, kernel=kernlab::vanilladot(), balancing_constraint=TRUE, s=0.0, x_center=TRUE, scale=FALSE, eps=1e-9,max_iter=20, verbose=FALSE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  y <- as.numeric(ModelVariables$Y[,1,drop=FALSE])*2-1
  
  L <- nrow(X)
  U <- nrow(X_u)
  
  if (!inherits(kernel,"kernel")) { stop("Not a valid kernel.")}
  
  Xe<-rbind(X,X_u,X_u)
  
  Ke <- kernlab::kernelMatrix(kernel,Xe)
  K <- Ke[1:L,1:L]
  # Calculate column for balancing constraint
  if (balancing_constraint) {
    constraintcol <- rowMeans(Ke[,(L+1):(L+U),drop=FALSE])
    Ke <- rbind(cbind(Ke,constraintcol),c(constraintcol,mean(Ke[(L+1):(L+U),(L+1):(L+U)])))
  }
  
  ## The Main CCCP algorithm
  sup <- solve_svm(K,y,C=C)
  f_pred <- (sup$alpha) %*% (Ke[1:L,]) + sup$b
  
  ysup <- y
  if (verbose) { 
    loss <- function(w) {
      
      X<-cbind(X,1)
      X_u<-cbind(X_u,1)
      y_u<-c(rep(-1,U), rep(1,U))
      y<-ysup
      tsvm_lin_loss(w,X,X_u,y,y_u,s,C,Cstar)
    }
    w <- c((sup$alpha) %*% X, sup$b)
    cat("Loss @ Start: ",loss(w),'\n')
    cat("w    @ Start: ",w,'\n')
  }
  
  if (balancing_constraint) {y_used <- c(y, rep(-1,U), rep(1,U),1) }
  else { y_used <- c(y, rep(-1,U), rep(1,U))}
  
   # The labeling that will be used, unlabeled objects are added twice, with both labels
  N <- L+2*U
  beta <- rep(0,2*U)
  beta[(y_used*f_pred)[(L+1):(L+2*U)]<s] <- Cstar

  if (balancing_constraint) { xi <- c(y_used[-length(y_used)], mean(y)) } # Include the balancing constraint
  else { xi <- y_used} 
  y <- y_used
  
  beta_prev<-rep(Inf,length(beta))
  iterations <- 0 
  while (norm(matrix(beta-beta_prev))>eps & iterations<(max_iter+1)) {
    
    
    #Define the constraints
    if (balancing_constraint) {
      Amat <- cbind(rep(1,N+1),
                    rbind(diag(y[1:L]),matrix(0,N-L+1,L)),
                    rbind(diag(-y[1:L]),matrix(0,N-L+1,L)),
                    rbind(matrix(0,L,N-L),diag(y[(L+1):N]),rep(0,N-L)),
                    rbind(matrix(0,L,N-L),diag(-y[(L+1):N]),rep(0,N-L))
                    )
    } else {
      Amat <- cbind(rep(1,N),
                    rbind(diag(y[1:L]),matrix(0,N-L,L)),
                    rbind(diag(-y[1:L]),matrix(0,N-L,L)),
                    rbind(matrix(0,L,N-L),diag(y[(L+1):N])),
                    rbind(matrix(0,L,N-L),diag(-y[(L+1):N]))
      )
    }
    
    bvec <- c(0,
              rep(0,L),
              rep(-C,L),
              -beta,
              beta-Cstar)
    
    #Solve for alpha
    res <- solve.QP(Dmat = (Ke+diag(0.00000001,nrow(Ke))), 
                         dvec = xi, Amat=Amat, bvec=bvec,meq=1)
    solution <- res
    alpha <- res$solution
    
    # SVQP2 solution
    # l<-c(ifelse(y==1,0,-C)[1:L],ifelse(y[(L+1):(L+2*U)]==1,-beta,-Cstar+beta),-1000000000)
    # u<-c(ifelse(y==1,C,0)[1:L],ifelse(y[(L+1):(L+2*U)]==1,Cstar-beta,beta),100000000)
    # runSVQP2((Ke+diag(0.00000001,nrow(Ke))),xi,l,u,verbosity=3,epskt=1e-2,maxcachesize = 1024*1024*256)
          
    eps <- 0.0000001
    #Compute the bias term
    w <- (alpha) %*% Ke
    
    act <- solution$iact-1
    act <- act[act>0]
    
    act_sup <- act[act <= 2*L]
    remove_sup <- ((act_sup-1) %% L) + 1
    
    act_semi <- (act)[act > 2*L]-2*L
    
    if (balancing_constraint) {
      remove_semi <- ((act_semi-1) %% (length(y)-(L+1)) + 1) + L
      idx <- c(remove_sup,remove_semi,length(y))
    } else {
      remove_semi <- ((act_semi-1) %% (length(y)-(L)) + 1) + L
      idx <- c(remove_sup,remove_semi)
    }
    #print(y[-idx]*(1-(w*y)[-idx]))
    
    if (length(idx) > 0) {
      b <- median(y[-idx]*(1-(w*y)[-idx]))
    } else {
      b <- 0
      warning("Could not find b in TSVM")
    }
    # Old method to find support vectors
    # ord<-order(pmin(
    #   c(abs((y*alpha)[c(1:L)]),
    #    abs((y*alpha)[(L+1):(N)]+beta)
    #   ),
    #   c(abs(C-(y*alpha)[c(1:L)]),
    #    abs(Cstar-((y*alpha)[(L+1):(N)]+beta))
    #   )
    # ),decreasing=TRUE)
    # 
    # b <- (y*(1-(w*y)))[ord[1:10]]
    # print(b)
    # if (abs(b[1]-b[2])>10e-4) warning("Problem finding b in TSVM solution")
    # b <- b[1]
    # 
    # vals<-c(
    #   y[c(1:L,N+1)][(y*alpha)[c(1:L)]>eps & (y*alpha)[c(1:L)]<(C-eps)]*(1-(w*y)[c(1:L,N+1)][(y*alpha)[c(1:L)]>eps & (y*alpha)[c(1:L)]<(C-eps)]),
    #   y[(L+1):(N)][(y*alpha)[(L+1):(N)]>-beta+eps & (y*alpha)[(L+1):(N)]<Cstar-eps-beta]*(1-(w*y)[(L+1):(N)][(y*alpha)[(L+1):(N)]>-beta+eps & (y*alpha)[(L+1):(N)]<Cstar-eps-beta])
    # )
    # if (length(vals)>0) {
    #   b <- median(vals,na.rm = TRUE)
    # } else {
    #   browser()
    # }
    
    if (verbose) {
      #cat("Possible Bias values: ", vals, "\n")
      if (balancing_constraint) { w <- c((alpha[-length(alpha)]) %*% Xe, b) }
      else { w <- c((alpha) %*% Xe, b) }
      cat("Loss @ iteration ",iterations,": ",loss(w),'\n')
      cat("w    @ iteration ",iterations,": ",w,'\n')
    }
    
    # Update Beta
    f_pred <- (alpha) %*% Ke + b
    beta_prev <- beta
    beta <- rep(0,2*U)
    beta[(y_used*f_pred)[(L+1):(L+2*U)]<s] <- Cstar
    
    iterations <- iterations + 1
  }
  return(new(Class = "TSVM",
             alpha=alpha,
             bias=b,
             Xtrain=Xe,
             kernel=kernel,
             beta=beta,
             labeled=L,
             intercept=FALSE,
             iterations=iterations,
             scaling=ModelVariables$scaling,
             modelform=ModelVariables$modelform,
             classnames=ModelVariables$classnames,
             balancing_constraint=balancing_constraint
             ))
}

#' @rdname decisionvalues-methods
#' @aliases decisionvalues,TSVM-method
setMethod("decisionvalues", signature(object="TSVM"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=object@intercept,classnames=object@classnames)
  X <- ModelVariables$X
  
  if (!is.null(object@kernel)) {
    K <- kernelMatrix(object@kernel,object@Xtrain,X)
    if (object@balancing_constraint) K <- rbind(K,colMeans(K[-c(1:object@labeled),,drop=FALSE]))
    output <- object@alpha %*% K + object@bias
  } else {
    K <- object@Xtrain %*% t(X)
    if (object@balancing_constraint) K <- rbind(K,colMeans(K[-c(1:object@labeled),,drop=FALSE]))
    output <- object@alpha %*% K + object@bias
  }
  
  return(as.numeric(output))
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
  alpha <- solution$solution * y
  w <- K %*% alpha
  idx <- ((solution$iact-2) %% length(alpha))+1
  bias <- y[-idx]*(1-(w*y)[-idx])
  #print(bias)
  bias <- median(bias)
  
  object<-list(b=bias,alpha=alpha)
  class(object)<-"SVM"
  return(object)
}

#' @title Linear CCCP Transductive SVM classifier
#'
#' @description Implementation for the Linear TSVM. This method is mostly for debugging purposes and does not allow for the balancing constraint or kernels, like the TSVM function.
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
#' @references Collobert, R. et al., 2006. Large scale transductive SVMs. Journal of Machine Learning Research, 7, pp.1687-1712.
#' @inheritParams BaseClassifier
#' @family RSSL classifiers
#' 
#' @export
LinearTSVM <- function(X, y, X_u, C, Cstar, s=0.0, x_center=FALSE, scale=FALSE, 
                       eps=1e-6, verbose=FALSE, init=NULL) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,
                                scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  y <- as.numeric(ModelVariables$Y[,1,drop=FALSE]*2-1)
  X_u <- ModelVariables$X_u


  L <- nrow(X)
  U <- nrow(X_u)
  
  y_u<-c(rep(-1,U),rep(1,U))
  Xe<-rbind(X,X_u,X_u)
  
  K <- X %*% t(X)
  
  sup <- solve_svm(K,y,C=C)
  if (is.null(init)) {
    w <- c((sup$alpha) %*% X, sup$b)
  } else {
    w <- init
  }
  
  w_prev <- rep(Inf,length(w))
  iterations <- 1 
  X<-cbind(X,1)
  X_u<-cbind(X_u,1)

  if (verbose) cat("Loss @ Start   :", tsvm_lin_loss(w,X,X_u,y,y_u,s,C,Cstar),"\n")
  if (verbose) cat("w    @ Start: ",w,'\n')
  
  while (norm(matrix(w-w_prev))>eps) {
    
    w_prev<-w
    w <- optim(w, fn=tsvm_cccp_lin_objective, 
               gr=tsvm_cccp_lin_gradient, 
               w_now=w, X=X,X_u=X_u,y=y,y_u=y_u,s=s,C=C,Cstar=Cstar)$par
    
    if (verbose) cat("Loss @ iter ",iterations,": ", tsvm_lin_loss(w,X,X_u,y,y_u,s,C,Cstar),"\n")
    if (verbose) cat("w    @ iter ",iterations,": ", w,"\n")
    iterations <- iterations + 1
  }
  b<-w[length(w)]
  
  return(new("LinearSVM",
             name="Linear Transductive Support Vector Machine",
             w=c(b,w[-length(w)]),
             scaling=ModelVariables$scaling,
             modelform=ModelVariables$modelform,
             classnames=ModelVariables$classnames))
}

# Adaptable Hinge Loss
hs <- function(x,s) {
  pmax(s-x,0)
} 

tsvm_lin_loss <- function(w,X,X_u,y,y_u,s,C,Cstar) { 
  0.5 * (t(w[-length(w)]) %*% w[-length(w)]) + 
    C * sum(hs(y *(X %*% w),s=1)) +
    Cstar * sum(hs(y_u *rbind(X_u,X_u) %*% w,s=1)) - 
    Cstar * sum(hs(y_u*rbind(X_u,X_u) %*% w, s=s))
}

tsvm_cccp_lin_objective <- function(w,w_now,X,X_u,y,y_u,s=0,C,Cstar) {
  w <- matrix(w,length(w))
  w_now <- matrix(w,length(w_now))
  
  0.5 * (t(w[-length(w)]) %*% w[-length(w)])  +
    C * sum(hs(y *(X %*% w),s=1)) +
    Cstar * sum(hs(y_u *(rbind(X_u,X_u) %*% w),s=1)) +
    - Cstar * colSums(-(y_u *(rbind(X_u,X_u)))[hs(y_u*(rbind(X_u,X_u) %*% w_now), s=s)>0,,drop=FALSE]) %*% w
}

tsvm_cccp_lin_gradient <- function(w,w_now,X,X_u,y,y_u,s,C,Cstar) {
  w <- matrix(w,length(w))
  w_now <- matrix(w,length(w_now))
  X_u2 <- rbind(X_u,X_u)
  
  c(w[-length(w)],0)  +
    - C * colSums(rbind(rep(0,ncol(X)),
                        (y*X)[(hs(y *(X %*% w),s=1)>0),])) +
    - Cstar * colSums(rbind(rep(0,ncol(X)),
                            (y_u * X_u2)[(hs(y_u *(X_u2 %*% w),s=1)>0),])) +
    - Cstar * colSums(-(y_u *(rbind(X_u,X_u)))[hs(y_u*(rbind(X_u,X_u) %*% w_now), s=s)>0,,drop=FALSE])
}
