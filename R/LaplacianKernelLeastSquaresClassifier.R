#' Laplacian Regularized Least Squares Classifier
#' 
#' Implements manifold regularization through the graph Laplacian as proposed by Belkin et al. 2006. As an adjacency matrix, we use the k nearest neighbour graph based on a chosen distance (default: euclidean).
#' 
#' @references Belkin, M., Niyogi, P. & Sindhwani, V., 2006. Manifold regularization: A geometric framework for learning from labeled and unlabeled examples. Journal of Machine Learning Research, 7, pp.2399-2434.
#' 
#' @family RSSL classifiers
#' 
#' @param gamma numeric; Weight of the unlabeled data
#' @inheritParams LaplacianSVM
#' @inheritParams BaseClassifier
#' @example inst/examples/example-LaplacianKernelLeastSquaresClassifier.R
#' @export
LaplacianKernelLeastSquaresClassifier <- function(X, y, X_u, lambda=0, gamma=0, kernel=kernlab::vanilladot(), adjacency_distance="euclidean", adjacency_k=6, x_center=TRUE, scale=TRUE, y_scale=TRUE, normalized_laplacian=FALSE) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  Y <- ModelVariables$Y[,1,drop=FALSE]
  
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
      W <- adjacency_knn(Xtrain,distance=adjacency_distance,k=adjacency_k)
      
      d <- rowSums(W)
      L <- diag(d) - W
      if (normalized_laplacian) {
        L <- diag(1/sqrt(d)) %*% L %*% diag(1/sqrt(d))
      }
      
      Y <- rbind(Y,matrix(0,u,ncol(Y)))
      J <- Matrix::bdiag(diag(l),0*diag(u))
      
      theta <- solve(J %*% K + lambda*diag(n)*l + (gamma*l/((l+u)^2))*L%*%K, Y)
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

#' Calculate knn adjacency matrix
#' 
#' Calculates symmetric adjacency: objects are neighbours is either one of them is in the set of nearest neighbours of the other.
#' 
#' @param X matrix; input matrix
#' @param distance character; distance metric used in the \code{dist} function
#' @param k integer; Number of neighbours
#' @return Symmetric binary adjacency matrix
adjacency_knn <- function(X,distance="euclidean",k=6) {
  
  Ds <- as.matrix(dist(X,method=distance))
  neighbours <- apply(Ds,1,function(x) sort(x,index.return=TRUE)$ix[2:(k+1)])  %>% as.integer
  adj <- as.matrix(Matrix::sparseMatrix(i=rep(1:nrow(X),each=k),
                                        j=neighbours,
                                        x=1,
                                        dims=c(nrow(X),nrow(X))))
  adj <- (adj | t(adj)) * 1
  
}