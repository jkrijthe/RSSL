#' Laplacian Regularized Least Squares Classifier
#' 
#' @param adjacency_kernel kernlab::kernel to use as adjacency kernel
#' @param gamma numeric; Weight of the unlabeled data
#' @inheritParams BaseClassifier
#' @example tests/examples/exampleLaplacianKernelLeastSquaresClassifier.R
#' @export
LaplacianKernelLeastSquaresClassifier <- function(X, y, X_u, lambda=0, gamma=0, kernel=vanilladot(), adjacency_kernel=rbfdot(1/4), x_center=TRUE, scale=TRUE, y_scale=TRUE) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  Y <- ModelVariables$Y
  
  ## Start Implementation
  l<-nrow(X)
  m<-ncol(X)
  u<-nrow(X_u)
  n <- l+u
  
  Xtrain<-NULL
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  if (y_scale) {
    y_scale <- colMeans(Y)
  } else {
    y_scale <- rep(0,ncol(Y))
  }
  
  Y <- sweep(Y,2,y_scale) # Possibly center the numeric Y labels
  
  if (inherits(kernel,"kernel")) {
    if (gamma>0) {
      Xtrain <- rbind(X,X_u)
      K <- kernelMatrix(kernel,Xtrain,Xtrain)
      W <- kernelMatrix(adjacency_kernel,Xtrain,Xtrain)
      L <- diag(rowSums(W)) - W
      Y <- rbind(Y,matrix(0,u,ncol(Y)))
      theta <- solve(Matrix::bdiag(diag(l),0*diag(u)) %*% K + lambda*diag(n)*l + (gamma*l/((l+u)^2))*L%*%K, Y)
      theta<-matrix(theta)
    } else {
      Xtrain <- X
      K <- kernelMatrix(kernel,Xtrain,Xtrain)
      theta <- solve(K+lambda*diag(l)*l, Y)
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
      y_scale=y_scale
  )
}