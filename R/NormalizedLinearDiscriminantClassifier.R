#' @include LinearDiscriminantClassifier.R
setClass("NormalizedLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Normalized Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Moment constriant semi-supervised linear discriminant analysis
#'
#' <full description>
#'
#' @param X <what param does>
#' @param y <what param does>
#' @param X_u <what param does>
#' @param method <what param does>
#' @param prior <what param does>
#' @param scale <what param does>
#' @param ... <what param does>
#' @export
NormalizedLinearDiscriminantClassifier <- function(X, y, X_u, method="closedform",prior=NULL, scale=FALSE,  ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  Xe<-rbind(X,X_u)
  
  #Set priors if not set by user
  if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
  
  #Calculate means for classes
  means<-t((t(X) %*% Y))/(colSums(Y))
  
  #Set sigma to be the average within scatter matrix
  sigma.classes<-lapply(1:ncol(Y),function(c,X){cov_ml(X[Y[,c]==1,])},X)
  sigma<-sigma.classes[[1]]*prior[1]
  for (i in 2:length(sigma.classes)) {
    sigma<-sigma+sigma.classes[[i]]*prior[i]
  }

  S_within_labeled<-sigma
  S_total_all<-cov(Xe)
  
  eigen(solve(S_total_all) %*% S_within_labeled)
  # Take first direction
  browser()
  e<-NULL #TODO
  
  # Project into new space
  
  # Look within-variance in first direction in the new space
  newX <- X %*% e
  sigma.classes<-lapply(1:ncol(Y),function(c,X){cov_ml(X[Y[,c]==1,])},newX)
  sigma<-sigma.classes[[1]]*prior[1]
  for (i in 2:length(sigma.classes)) {
    sigma<-sigma+sigma.classes[[i]]*prior[i]
  }
  v<-sigma
  
  # Look at variance in the null space of the new lda projection vector
  Ne<-Null(e)
  newX <- X %*% Ne
  sigma.classes<-lapply(1:ncol(Y),function(c,X){cov_ml(X[Y[,c]==1,])},newX)
  sigma<-sigma.classes[[1]]*prior[1]
  for (i in 2:length(sigma.classes)) {
    sigma<-sigma+sigma.classes[[i]]*prior[i]
  }
  C<-sigma
  
  #Find new covariance matrix
  sigma<-list()
  sigma[[1]]<-v * e %*% t(e) + t(Ne) %*% C %*% Ne
  sigma[[2]]<-sigma[[1]]
  
  new("NormalizedLinearDiscriminantClassifier", prior=prior, means=means, sigma=sigma,classnames=classnames,scaling=scaling, modelform=modelform)
}
