#' @include LinearDiscriminantClassifier.R
setClass("MCLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Moment Constrained Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Moment constriant semi-supervised linear discriminant analysis
#'
#' <full description>
#'
#' @usage MCLinearDiscriminantClassifier(X, y, X_u, method="closedform",prior=NULL, scale=FALSE,  ...)
#'
#' @param X <what param does>
#' @param y <what param does>
#' @param X_u <what param does>
#' @param method <what param does>
#' @param prior <what param does>
#' @param scale <what param does>
#' @param ... <what param does>
#' @export
MCLinearDiscriminantClassifier <- function(X, y, X_u, method="closedform",prior=NULL, scale=FALSE,  ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
    #Set priors if not set by user
    if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
    
    #Calculate means for classes
    means<-t((t(X) %*% Y))/(colSums(Y))
    
    #Set sigma to be the average within scatter matrix
    sigma.classes<-lapply(1:ncol(Y),function(c,X){cov(X[Y[,c]==1,])},X)
    sigma<-sigma.classes[[1]]*prior[1]
    for (i in 2:length(sigma.classes)) {
      sigma<-sigma+sigma.classes[[i]]*prior[i]
    }
    
    
    T.labeled<-cov(X)
    T.all<-cov(rbind(X,X_u))
    m.labeled<-colMeans(X)
    m.all<-colMeans(rbind(X,X_u))
    
    
    matrixsqrt <- function(X) {
      decomposition<-svd(X)
      decomposition$u %*% diag(sqrt(decomposition$d)) %*% decomposition$v
    }

    
    sigma <- matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled)) %*% sigma %*% matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled))
    means <- t(matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled)) %*% t(means-matrix(1,nrow(means),1) %*% m.labeled ) + t(matrix(1,nrow(means),1) %*% m.all)) 
    
    sigma<-lapply(1:ncol(Y),function(c){sigma})
    
  } else if (method=="ml") {
    
  }
  new("MCLinearDiscriminantClassifier", prior=prior, means=means, sigma=sigma,classnames=classnames,scaling=scaling, modelform=modelform)
}
