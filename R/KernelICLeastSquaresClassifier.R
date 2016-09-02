#' @include KernelLeastSquaresClassifier.R
setClass("KernelICLeastSquaresClassifier", 
         representation(responsibilities="ANY"), 
         prototype(name="KernelICLeastSquaresClassifier"), 
         contains="KernelLeastSquaresClassifier")

#' Kernelized Implicitly Constrained Least Squares Classification
#' 
#' A kernel version of the implicitly constrained least squares classifier, see \code{\link{ICLeastSquaresClassifier}}.
#' 
#' @param lambda_prior numeric; regularization parameter for the posterior deviation from the prior
#' @param projection character; The projection used. One of c("supervised","semisupervised")
#' @param method character; Estimation method. One of c("LBFGS")
#' @param classprior The classprior used to compare the estimated responsilibities to
#' @inheritParams BaseClassifier
#' @export
KernelICLeastSquaresClassifier <- function(X, y, X_u, lambda=0, kernel=vanilladot(), x_center=TRUE, scale=TRUE, y_scale=TRUE, lambda_prior=0, classprior=0, method="LBFGS", projection="semisupervised") {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  if (ncol(ModelVariables$Y)==2) {
    Y <- ModelVariables$Y[,1,drop=FALSE]
  } else {
    Y <- ModelVariables$Y
  }
  
  stopifnot(ncol(Y)==1)
  
  ## Start Implementation
  n<-nrow(X)
  m<-ncol(X)
  
  Xtrain<-NULL
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  if (y_scale) {
    y_scale <- colMeans(Y)
    classprior <- classprior-y_scale
  } else {
    y_scale <- rep(0,ncol(Y))
  }
  
  Y <- sweep(Y,2,y_scale) # Possibly center the numeric Y labels
  
  if (inherits(kernel,"kernel")) {
    Xtrain <- rbind(X, X_u)
    K <- kernlab::kernelMatrix(kernel,Xtrain,Xtrain)
    alpha_sup <- solve(K[1:n,1:n,drop=FALSE]+lambda*diag(n), Y)
    
    theta <- rep(0.5,nrow(X_u))
    Kinv <- inv(K+1e-6*diag(nrow(K)))
    if (method=="LBFGS") {
      if (projection=="supervised") {

        opt_result <- optim(theta, objective_kicls, gr=gradient_kicls, 
                            Kinv=Kinv, K=K, alpha_sup=alpha_sup, l=n, lambda_prior=lambda_prior,classprior=classprior,Y=Y, 
                            method="L-BFGS-B", lower=0.0-y_scale, upper=1.0-y_scale, control=list(fnscale=1))
        unlabels <- opt_result$par
        theta <- matrix(Kinv %*% c(as.numeric(Y), unlabels))
      } else if (projection=="semisupervised") {

        opt_result <- optim(theta, objective_kicls_semi, gr=gradient_kicls_semi, 
                            K=K, alpha_sup=alpha_sup, n=n, lambda_prior=lambda_prior,classprior=classprior, Y=Y, Kinv=Kinv, 
                            method="L-BFGS-B", lower=0.0-y_scale, upper=1.0-y_scale, control=list(fnscale=1))
        unlabels <- opt_result$par
        theta <- matrix(solve(K+diag(nrow(K))*0.00000001, c(as.numeric(Y), unlabels)))
      } else {
        stop("Projection does not exist.")
      }
    }
    
  } else {
    stop("No appropriate kernel function from kernlab supplied. See, for instance, the help of vanilladot()")
  }
  
  ## Return correct object
  new("KernelICLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      kernel=kernel,
      Xtrain=Xtrain,
      y_scale=y_scale,
      responsibilities=unlabels,
      optimization=opt_result
  )
}

objective_kicls_semi <- function(resp,K,alpha_sup,n,lambda_prior,classprior,Y,Kinv) {
  alpha_sup <- matrix(alpha_sup,nrow = 1)
  resp <- matrix(resp,ncol=1)
  #t(resp) %*% resp - 2 * alpha_sup %*% K[1:n,-c(1:n)] %*% resp + lambda_prior*(mean(resp)-classprior)^2
  
  # Alternative
  t(resp) %*% Kinv[-c(1:n),] %*% K %*% K %*% Kinv[, -c(1:n)] %*% resp + 
    2* t(resp) %*% Kinv[-c(1:n),] %*% K %*% K %*% Kinv[, c(1:n)] %*% Y - 
    2 * alpha_sup %*% K[1:n,-c(1:n)] %*% resp
}

gradient_kicls_semi <- function(resp,K,alpha_sup,n,lambda_prior,classprior,Y,Kinv) {
  alpha_sup <- matrix(alpha_sup,nrow = 1)
  resp <- matrix(resp,ncol=1)
  # gradient <- 2 * t(resp) - 2 * alpha_sup %*% K[1:n,-c(1:n)] + lambda_prior*2*(mean(resp)-classprior)/nrow(resp)
#   print(gradient)
#   print(sum(abs(grad(objective_kicls_semi,resp,K=K,alpha_sup=alpha_sup,n=n,lambda_prior=lambda_prior,classprior=classprior)-gradient)))
  # gradient
  
  # Alternative
  2 * t(resp) %*% Kinv[-c(1:n),] %*% K %*% K %*% Kinv[, -c(1:n)] + 
    2* t(Kinv[-c(1:n),] %*% K %*% K %*% Kinv[, c(1:n)] %*% Y) - 
    2 * alpha_sup %*% K[1:n,-c(1:n)]
}

objective_kicls <- function(resp, Kinv, K, alpha_sup,Y,l,lambda_prior,classprior) {
  resp <- matrix(resp)
  reg_prior <- lambda_prior*(mean(resp)-classprior)^2
  objective <- t(resp) %*% Kinv[-c(1:l),] %*% K[,1:l] %*% K[1:l,] %*% Kinv[,-c(1:l)] %*% resp +
    2 * t(Y) %*% Kinv[c(1:l),] %*% K[,1:l] %*% K[1:l,] %*%Kinv[,-c(1:l)] %*% resp -
    2 * t(alpha_sup) %*% K[1:l,1:l] %*% K[1:l,] %*% Kinv[,-c(1:l)] %*% resp + 
    reg_prior
}

gradient_kicls <- function(resp, Kinv, K, alpha_sup, Y, l,lambda_prior,classprior) {
  resp <- matrix(resp)
  reg_prior <- lambda_prior*2*(mean(resp)-classprior)/nrow(resp)
  2 * t(resp) %*% Kinv[-c(1:l),] %*% K[,1:l] %*% K[1:l,] %*%Kinv[,-c(1:l)] +
    2 * t(Y) %*% Kinv[c(1:l),] %*% K[,1:l] %*% K[1:l,] %*%Kinv[,-c(1:l)] -
    2 * t(alpha_sup) %*% K[1:l,1:l] %*% K[1:l,] %*% Kinv[,-c(1:l)] +
    reg_prior
}
