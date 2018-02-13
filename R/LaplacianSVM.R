#' @include Classifier.R
setClass("LaplacianSVM",
         representation(scaling="ANY",alpha="ANY",bias="ANY",kernel="ANY",Xtrain="ANY",intercept="ANY",time="ANY"),
         prototype(name="Support Vector Machine"),
         contains="Classifier")

#' Laplacian SVM classifier
#' 
#' Manifold regularization applied to the support vector machine as proposed in Belkin et al. (2006). As an adjacency matrix, we use the k nearest neighbour graph based on a chosen distance (default: euclidean). 
#'
#' @references Belkin, M., Niyogi, P. & Sindhwani, V., 2006. Manifold regularization: A geometric framework for learning from labeled and unlabeled examples. Journal of Machine Learning Research, 7, pp.2399-2434.
#'
#' @family RSSL classifiers
#'
#' @param adjacency_k integer; Number of of neighbours used to construct adjacency graph.
#' @param adjacency_distance character; distance metric used to construct adjacency graph from the dist function. Default: "euclidean"
#' @param normalized_laplacian logical; If TRUE use the normalized Laplacian, otherwise, the Laplacian is used
#' @param gamma numeric; Weight of the unlabeled data
#' @param eps numeric; Small value to ensure positive definiteness of the matrix in the QP formulation
#' @inheritParams BaseClassifier
#' @return S4 object of type LaplacianSVM
#'
#' @example inst/examples/example-LaplacianSVM.R
#' @export
LaplacianSVM<-function(X, y, X_u=NULL, lambda=1, gamma=1, scale=TRUE, kernel=vanilladot(), adjacency_distance="euclidean", adjacency_k=6,normalized_laplacian=FALSE,eps=10e-10) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X, y=y, X_u=X_u, scale=scale, intercept=FALSE, x_center=TRUE)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  Y <- ModelVariables$Y[,1,drop=FALSE]
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  
  # Check for two classes and transform to {-1,1}
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  y <- as.numeric((Y*2)-1)
  
  ## Start Implementation
  l <- nrow(X)
  m <- ncol(X)
  u <- nrow(X_u)
  n <- l+u
  
  if (inherits(kernel,"kernel")) {
      Xtrain <- rbind(X,X_u)
      K <- kernelMatrix(kernel,Xtrain,Xtrain)

      W <- adjacency_knn(Xtrain,distance=adjacency_distance,k=adjacency_k)
      
      d <- rowSums(W)
      L <- diag(d) - W
      if (normalized_laplacian) {
        L <- diag(1/sqrt(d)) %*% L %*% diag(1/sqrt(d))
      }
      
      Y <- diag(y)
      J <- cbind(diag(l),matrix(0,l,u))
      #theta <- solve(Matrix::bdiag(diag(l),0*diag(u)) %*% K + lambda*diag(n)*l + (gamma*l/((l+u)^2))*L%*%K, Y)
      #theta<-matrix(theta)
      # Find beta
      Qprime <-  solve(2 * lambda * diag(l+u) + 2 * (gamma/((l+u)^2)) * L %*% K, t(J) %*% Y)
      Q <- Y %*% J %*% K %*% Qprime
      
      Amat <- diag(nrow(X))
      Amat <- t(rbind(y,Amat,-Amat))
      bvec <- c(rep(0,nrow(X)+1),rep(-1/l,nrow(X)))
      
      beta <- solve.QP(Q+diag(l)*eps, rep(1,l), Amat, bvec, meq=1)$solution
      if (any(is.nan(beta))) stop("Quadratic Programming problem returned: NaN. Try different hyperparameters?")
      
      alpha <- (Qprime %*% beta)
      
      SVs <- (beta >0.00000001/l) & (beta < 1/l - 0.00000001/l)
      C <- 1/(2*lambda*l)
      SVplus <- (alpha > 0.1) & (alpha < C-0.1)
      SVmin <- (alpha < -0.1) & (alpha > -C+0.1)
      
      bias <- median(K[(1:l)[SVs],] %*% alpha - y[SVs])
      
      bias <- 0
      if (sum(SVplus)>0 && sum(SVmin)>0) {
        bias <- -median(c(K[SVmin,] %*% alpha + 1,K[SVplus,] %*% alpha - 1))
      } 
      else {
        if (sum(SVplus)>0) {
          bias <- -median(K[SVplus,] %*% alpha - 1)
        }
        if (sum(SVmin)>0) {
          bias <- -median(K[SVmin,] %*% alpha + 1)
        }
      }
      #print(K[(1:l)[SVs],] %*% alpha - y[SVs])
      bias <- -median(K[(1:l)[SVs],] %*% alpha - y[SVs])
      
  } else {
    stop("No appropriate kernel function from kernlab supplied. See, for instance, the help of vanilladot()")
  }
  
  return(new("SVM",
             alpha=as.numeric(alpha),
             bias=bias,
             Xtrain=Xtrain,
             kernel=kernel,
             scaling=scaling,
             modelform=modelform,
             classnames=classnames,
             intercept=FALSE,
             name="Laplacian SVM"))
}