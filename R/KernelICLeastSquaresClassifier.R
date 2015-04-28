#' @include KernelLeastSquaresClassifier.R
setClass("KernelICLeastSquaresClassifier", 
         representation(unlabels="ANY"), 
         prototype(name="KernelICLeastSquaresClassifier"), 
         contains="KernelLeastSquaresClassifier")

#' @export
KernelICLeastSquaresClassifier <- function(X, y, X_u, lambda=0, kernel=vanilladot(), x_center=TRUE, scale=TRUE, y_scale=TRUE, lambda_prior=0, classprior=NULL, method="LBFGS", projection="semisupervised") {
  
  stopifnot(require(kernlab))
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  Y <- ModelVariables$Y
  
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
    Ksup <- kernelMatrix(kernel,X,X)
    alpha_sup <- solve(Ksup+lambda*diag(n)*n, Y)
    Xtrain <- rbind(X, X_u)
    K <- kernelMatrix(kernel,Xtrain,Xtrain)
    theta <- rep(0.5,nrow(X_u))
    if (method=="LBFGS") {
      if (projection=="supervised") {
        
        Kinv <- inv(K+1e-6*diag(nrow(K)))
        theta <- rep(0.5,nrow(X_u))
        
        opt_result <- optim(theta, objective_kicls, gr=gradient_kicls, Kinv=Kinv, K=K, alpha_sup=alpha_sup, l=n, lambda_prior=lambda_prior,classprior=classprior,Y=Y, method="L-BFGS-B", lower=0.0-y_scale, upper=1.0-y_scale, control=list(fnscale=1))
        theta<-opt_result$par
        unlabels<-theta
        theta <- matrix(Kinv %*% c(as.numeric(Y), unlabels))
      }
      else if (projection=="semisupervised") {
        theta <- rep(0.5,nrow(X_u))
        
        opt_result <- optim(theta, objective_kicls_semi, gr=gradient_kicls_semi, K=K, alpha_sup=alpha_sup, n=n, lambda_prior=lambda_prior,classprior=classprior, method="L-BFGS-B", lower=0.0-y_scale, upper=1.0-y_scale, control=list(fnscale=1))
        unlabels <- theta
        theta <- matrix(solve(K+diag(nrow(K))*0.00000001, c(as.numeric(Y), unlabels)))
      }
    }
    
  } else {
    stop("No appropriate kernel function from kernlab supplied. See, for instance, the help of vanilladot()")
  }
  
  ## Return correct object
  new("KernelLeastSquaresClassifier",
      classnames=classnames,
      scaling=scaling,
      theta=theta,
      modelform=modelform,
      kernel=kernel,
      Xtrain=Xtrain,
      y_scale=y_scale,
      unlabels=unlabels
  )
}

objective_kicls_semi <- function(resp,K,alpha_sup,n,lambda_prior,classprior) {
  alpha_sup <- matrix(alpha_sup,nrow = 1)
  resp <- matrix(resp,ncol=1)
  t(resp) %*% resp - 2 * alpha_sup %*% K[1:n,-c(1:n)] %*% resp + lambda_prior*(mean(resp)-classprior)^2
}

gradient_kicls_semi <- function(resp,K,alpha_sup,n,lambda_prior,classprior) {
  alpha_sup <- matrix(alpha_sup,nrow = 1)
  resp <- matrix(resp,ncol=1)
  gradient <- 2 * t(resp) - 2 * alpha_sup %*% K[1:n,-c(1:n)] + lambda_prior*2*(mean(resp)-classprior)/nrow(resp)
  #print(sum(abs(grad(objective_kicls_semi,resp,K=K,alpha_sup=alpha_sup,n=n,lambda_prior=lambda_prior,classprior=classprior)-gradient)))
  gradient
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
