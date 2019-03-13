#' @include Classifier.R
NULL

#' LinearSVM Class  
setClass("LinearSVM",
         representation(w="ANY",scaling="ANY",time="ANY",opt_result="ANY"),
         prototype(name="Support Vector Machine"),
         contains="Classifier")

#' Linear SVM Classifier
#' 
#' Implementation of the Linear Support Vector Classifier. Can be solved in the Dual formulation, which is equivalent to \code{\link{SVM}} or the Primal formulation.
#' 
#' @family RSSL classifiers
#' 
#' @param C Cost variable
#' @param method Estimation procedure c("Dual","Primal","BGD")
#' @param scale Whether a z-transform should be applied (default: TRUE)
#' @param eps Small value to ensure positive definiteness of the matrix in QP formulation
#' @param reltol relative tolerance using during BFGS optimization
#' @param maxit Maximum number of iterations for BFGS optimization
#' @inheritParams BaseClassifier
#' 
#' @return S4 object of type LinearSVM
#' 
#' @export
LinearSVM<-function(X, y, C=1, method="Dual", scale=TRUE, eps=1e-9, reltol=10e-14, maxit=100) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables <- PreProcessing(X,y,scale=scale,intercept=TRUE,x_center=TRUE)
  X <- ModelVariables$X
  y <- ModelVariables$y
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  Y <- ModelVariables$Y[,1,drop=FALSE]
  intercept <- TRUE
  opt_result <- NULL 
  y <- as.numeric((Y*2)-1)

  ## Start Implementation
  time.begin<-Sys.time()
  if (method=="Dual") {
    
    if (intercept) X <- X[,2:ncol(X)]
    Dmat <- (diag(y) %*% X %*% t(diag(y) %*% X)) + eps*diag(nrow(X)) #Add small constant to diagonal to ensure numerical PSD
    dvec <- matrix(1, nrow(X), 1)
    Amat <- diag(nrow(X))
    Amat <- t(rbind(y,Amat,-Amat))
    bvec <- c(rep(0,nrow(X)+1),rep(-C,nrow(X)))
    
    opt_result <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    alpha<-opt_result$solution
    w <- matrix(alpha*y,1,nrow(X)) %*% X
    
    idx <- ((opt_result$iact-2) %% length(alpha))+1
    
    b <- X[-idx,,drop=FALSE] %*% t(w) - y[-idx]
    b <- -median(b)
    w<-c(b, w)
    
  } else if (method=="Primal") {
    
    Dmat<-Matrix::bdiag(matrix(0,1,1),diag(ncol(X)-1),matrix(0,nrow(X),nrow(X))) + eps*diag(ncol(X)+nrow(X))
    dvec <- c(rep(0,ncol(X)), rep(C,nrow(X)))
    Amat <- cbind(matrix(0,nrow(X),ncol(X)),diag(nrow(X))) #Slack variable bigger than 0
    Amat <- rbind(Amat,cbind(diag(y) %*% X, diag(nrow(X))))
    Amat <- t(Amat)
    bvec <- c(rep(0,nrow(X)), rep(1,nrow(X)))            
    opt_result <- solve.QP(Dmat, -dvec, Amat, bvec)
    w <- opt_result$solution[1:ncol(X)]
    
  } else if (method=="BGD") {
    # Give a warning that this method does not work as expected
    warning("BFGS might not converge to the optimal solution.")
    
    w0 <- rep(0.0, ncol(X)) #Initial parameter values
    
    opt_result <- optim(w0, svm_opt_func, gr=svm_opt_grad, X=X, y=y, C=C,eps=0,method="BFGS",control=list(reltol=reltol,maxit=maxit))
    
    w <- opt_result$par
    
    # Try reducing the C parameter
    Cnew <-C
    while(svm_opt_grad(w,X,y,C,eps)[1]==0.0) {
      Cnew<-Cnew/2
      w <- optim(w, svm_opt_func, X=X, y=y, C=Cnew,eps=0,control=list(reltol=1e-30,maxit=maxit))$par
    }
    w <- optim(w, svm_opt_func, gr=svm_opt_grad, X=X, y=y, C=C,eps=0,method="BFGS",control=list(reltol=reltol,maxit=maxit))$par
    
    #print(svm_opt_grad(w,X,y,C,eps))
    #print(numDeriv::grad(svm_opt_grad,w,X=X,y=y,C=C,eps=eps,method="simple"))
    

  } else {
    stop("Unknown optimization method.")
  }
  
  time.passed<-Sys.time()-time.begin
  
  return(new("LinearSVM",
             w=w,
             scaling=scaling,
             modelform=modelform,
             classnames=classnames,
             time=time.passed,
             opt_result=opt_result))
}

#' @rdname decisionvalues-methods
#' @aliases decisionvalues,LinearSVM-method
setMethod("decisionvalues", signature(object="LinearSVM"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=TRUE)
  X <- ModelVariables$X
  w <- matrix(object@w,nrow=ncol(X))
  
  return(as.numeric(X %*% w))
})


#' @rdname rssl-predict
#' @aliases predict,LinearSVM-method
setMethod("predict", signature(object="LinearSVM"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=TRUE)
  X<-ModelVariables$X
  
  w <- matrix(object@w,nrow=ncol(X))
  
  result<-factor(as.numeric(X %*% w<0),levels=0:1,labels=object@classnames)
  return(result)
})

#' Loss method for LinearSVM
#'
#' Hinge loss on new objects of a trained LinearSVM
#' @rdname loss-methods
#' @aliases loss,LinearSVM-method
setMethod("loss", signature(object="LinearSVM"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,object@scaling,intercept=TRUE,classnames=object@classnames)
  X <- ModelVariables$X
  Y <- ModelVariables$Y[,1,drop=FALSE]
  y <- as.numeric((Y*2)-1)
  
  w <- matrix(object@w,nrow=ncol(X))
  d <- 1 - y * (X %*% w)
  d[d<0] <- 0
  return(as.numeric(d))
})

#' @rdname line_coefficients-methods
#' @aliases line_coefficients,LinearSVM-method 
setMethod("line_coefficients", signature(object="LinearSVM"), function(object) {
  return(coefficients_after_scaling(w0=object@w[1],w=object@w[2:3],scaling=object@scaling))
})

svm_opt_func <- function(w, X, y, C, eps=0.0) {
  d <- 1 - y * (X %*% w)
  l <- C * sum(d[d>0]) +  0.5 * w[-1] %*% w[-1] + eps
  return(as.numeric(l))
}

svm_opt_grad <- function(w, X, y, C, eps=0.0) {
  d <- 1 - y * (X %*% w)
  grad <- - y * X
  if (sum(d>0)>0) {
    grad <- C * colSums(grad[d>0,,drop=FALSE]) + c(0,w[-1]+eps)
  } else {
    grad <-  c(0,w[-1]+eps)
  }
  return(grad)
}

