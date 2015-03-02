#' @export
KernelICLeastSquaresClassifier <- function(X, y, X_u, lambda=0, kernel=vanilladot(), x_center=TRUE, scale=TRUE, y_scale=TRUE) {
  
  stopifnot(require(kernlab))
  
  ## Preprocessing to correct datastructures and scaling  
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
  } else {
    y_scale <- rep(0,ncol(Y))
  }
  
  Y <- sweep(Y,2,y_scale) # Possibly center the numeric Y labels
  
  if (inherits(kernel,"kernel")) {
    Ksup <- kernelMatrix(kernel,X,X)
    alpha_sup <- solve(Ksup+lambda*diag(n)*n, Y)
    Xtrain <- rbind(X, X_u)
    K <- kernelMatrix(kernel,Xtrain,Xtrain)
    print(alpha_sup)

    opt_func_projection <- function(resp,K) {
      alpha_sup <- matrix(alpha_sup,nrow = 1)
      resp <- matrix(resp,ncol=1)
      t(resp) %*% resp - 2 * alpha_sup %*% K[1:n,-c(1:n)] %*% resp
    }
    
    opt_grad_projection <- function(resp,K) {
      alpha_sup <- matrix(alpha_sup,nrow = 1)
      resp <- matrix(resp,ncol=1)
      2 * t(resp) - 2 * alpha_sup %*% K[1:n,-c(1:n)]
    }
    theta <- rep(0.5,nrow(X_u))
    
    # Bounded optimization
    opt_result <- optim(theta, opt_func_projection, gr=opt_grad_projection, K=K, method="L-BFGS-B", lower=0.0-y_scale, upper=1.0-y_scale, control=list(fnscale=1))
    theta <- opt_result$par
    unlabels <- theta
    theta <- matrix(solve(K+diag(nrow(K))*0.00000001, c(as.numeric(Y), unlabels)))
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
