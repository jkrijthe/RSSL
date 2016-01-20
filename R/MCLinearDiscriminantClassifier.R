#' @include LinearDiscriminantClassifier.R
setClass("MCLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Moment Constrained Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Moment Constrained Semi-supervised Linear Discriminant Analysis.
#'
#' Using an update of the estimated means and covariance proposed in (Loog 2014). 
#' Using the method="closedform" option, uses the ad hoc parameter update proposed in (Loog 2014). The alternative way to minimize this objective function directly has not been implemented yet.
#' @references Loog, M., 2014. Semi-supervised linear discriminant analysis through moment-constraint parameter estimation. Pattern Recognition Letters, 37, pp.24-31.
#'
#' @param method character; Currently only "closedform"
#' @param prior Matrix (k by 1) of class prior probabilities. If NULL, estimated from data
#' @inheritParams BaseClassifier
#' 
#' @family RSSL classifiers
#' @export
MCLinearDiscriminantClassifier <- function(X, y, X_u, method="closedform",prior=NULL, x_center=FALSE, scale=FALSE) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  y <- ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
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
    
    means <- t(matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled)) %*% t(means-matrix(1,nrow(means),1) %*% m.labeled ) + t(matrix(1,nrow(means),1) %*% m.all))
    sigma <- ginv(matrixsqrt(T.labeled)) %*% matrixsqrt(T.all) %*%  sigma  %*% matrixsqrt(T.all) %*% ginv(matrixsqrt(T.labeled))
    
    
    sigma<-lapply(1:ncol(Y),function(c){sigma})
    
  } else if (method=="ml") {
    stop("Not implemented yet")
  }
  new("MCLinearDiscriminantClassifier", 
      prior=prior, 
      means=means, 
      sigma=sigma,
      classnames=classnames,
      scaling=scaling, 
      modelform=modelform)
}
