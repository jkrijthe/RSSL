#' @include LinearDiscriminantClassifier.R
setClass("MCLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Moment Constrained Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Moment Constrained Semi-supervised Linear Discriminant Analysis.
#' 
#' A linear discriminant classifier that updates the estimates of the means and covariance matrix based on unlabeled examples.
#' 
#' This method uses the parameter updates of the estimated means and covariance proposed in (Loog 2014). Using the method="invariant" option, uses the scale invariant parameter update proposed in (Loog 2014), while method="closedform" using the non-scale invariant version from (Loog 2012).
#' @references Loog, M., 2012. Semi-supervised linear discriminant analysis using moment constraints. Partially Supervised Learning, LNCS, 7081, pp.32-41.
#' @references Loog, M., 2014. Semi-supervised linear discriminant analysis through moment-constraint parameter estimation. Pattern Recognition Letters, 37, pp.24-31.
#'
#' @param method character; One of c("invariant","closedform")
#' @param prior Matrix (k by 1); Class prior probabilities. If NULL, estimated from data
#' @inheritParams BaseClassifier
#' @family RSSL classifiers
#' @export
MCLinearDiscriminantClassifier <- function(X, y, X_u, method="invariant", prior=NULL, x_center=TRUE, scale=FALSE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  y <- ModelVariables$y
  Y <- ModelVariables$Y
  
  #Set priors if not set by user
  if (is.null(prior)) prior <- matrix(colMeans(Y),2,1)
  
  #Calculate means for classes
  means<-t((t(X) %*% Y))/(colSums(Y))
  
  #Set sigma to be the average within scatter matrix
  sigma.classes<-lapply(1:ncol(Y),function(c,X){cov_ml(X[Y[,c]==1,,drop=FALSE])},X)
  sigma<-sigma.classes[[1]]*prior[1]
  for (i in 2:length(sigma.classes)) {
    sigma<-sigma+sigma.classes[[i]]*prior[i]
  }
  
  T.labeled<-cov_ml(X)
  T.all<-cov_ml(rbind(X,X_u))
  m.labeled<-colMeans(X)
  m.all<-colMeans(rbind(X,X_u))
  
  matrixsqrt <- function(X) {
    decomposition <- eigen(X)
    decomposition$vectors %*% diag(sqrt(decomposition$values)) %*% t(decomposition$vectors)
  }
  
  ginv <- svdinv
  
  if (method=="closedform") {
    
    means <- t(matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled)) %*% t(means-matrix(1,nrow(means),1) %*% m.labeled ) + t(matrix(1,nrow(means),1) %*% m.all))
    sigma <- ginv(matrixsqrt(T.labeled)) %*% matrixsqrt(T.all) %*%  sigma  %*% matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled))
    
    sigma<-lapply(1:ncol(Y),function(c){sigma})
    
  } else if (method=="invariant") {
    res <- svdeig(T.all,T.labeled)
    E <- res$E
    D <- res$D
    Trans <- t(svdinv(E)) %*% svdsqrtm(D) %*% t(E)
    
    ## Different ways of solving this:
      # browser()
      # E1 <- svdeig(T.all,T.labeled)$E
      # E2 <- svdeig(T.labeled,T.all)$E
      # D1 <- svdeig(T.all,T.labeled)$D
      # D2 <- svdeig(T.labeled,T.all)$D
      # E3 <- (eigen(ginv(T.all) %*% T.labeled)$vectors)
      # E4 <- (eigen(ginv(T.labeled) %*% T.all)$vectors)
      # 
      # t(svdinv(E1)) %*% svdsqrtm(D1) %*% t(E1)
      # t(svdinv(E2)) %*% svdinvsqrtm(D2) %*% t(E2)
      # 
      # E <- E3
      # D_T <- t(E) %*% T.labeled %*% E
      # D_theta <- t(E) %*% T.all %*% E
      # t(ginv(E)) %*% t(matrixsqrt(D_theta)) %*% ginv(matrixsqrt(D_T)) %*% t(E)
      # 
      # E <- E4
      # D_T <- t(E) %*% T.labeled %*% E
      # D_theta <- t(E) %*% T.all %*% E
      # t(ginv(E)) %*% t(matrixsqrt(D_theta)) %*% ginv(matrixsqrt(D_T)) %*% t(E)
    
    ## end
                              
    means <- t(Trans %*% t(means-matrix(1,nrow(means),1) %*% m.labeled ) + 
                 t(matrix(1,nrow(means),1) %*% m.all))
    sigma <- Trans %*%  sigma  %*% t(Trans)
    
    sigma<-lapply(1:ncol(Y),function(c){sigma})
    
  } else {
    stop("Value of method not recognized.")
  }
  
  new("MCLinearDiscriminantClassifier", 
      prior=prior, 
      means=means, 
      sigma=sigma,
      scaling=ModelVariables$scaling,
      classnames=ModelVariables$classnames,
      modelform=ModelVariables$modelform)
}
