#' @include LinearDiscriminantClassifier.R
setClass("ICLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix",responsibilities="ANY"),
         prototype(name="Implicitly Constrained Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Implicitly Constrained Semi-supervised Linear Discriminant Classifier
#'
#' Semi-supervised version of Linear Discriminant Analysis using implicit constraints as described in (Krijthe & Loog 2014). This method finds the soft labeling of the unlabeled objects, whose resulting LDA solution gives the highest log-likelihood when evaluated on the labeled objects only. See also \code{\link{ICLeastSquaresClassifier}}.
#'
#' @references Krijthe, J.H. & Loog, M., 2014. Implicitly Constrained Semi-Supervised Linear Discriminant Analysis. In International Conference on Pattern Recognition. Stockholm, pp. 3762-3767.
#'
#' @family RSSL classifiers
#'
#' @param X design matrix of the labeled objects
#' @param y vector with labels
#' @param X_u design matrix of the labeled objects
#' @param prior set a fixed class prior
#' @param scale logical; Should the features be normalized? (default: FALSE)
#' @param init not currently used
#' @param sup_prior logical; use the prior estimates based only on the labeled data, not the imputed labels (default: FALSE)
#' @param x_center logical; Whether the data should be centered
#' @param ... Additional Parameters, Not used
#' 
#' @export
ICLinearDiscriminantClassifier <- function(X, y, X_u, prior=NULL, scale=FALSE,init=NULL, sup_prior=FALSE, x_center=FALSE,...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  Y<-ModelVariables$Y
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  k<-ncol(X) # Number of features
  
  # Initialize responsibilities
#   g <- runif(nrow(X_u)) # Random uniform
#   g <- rep(1,nrow(X_u)) # All 1
  # Initialize responsibilities using supervised posterior
  g <- posterior(LinearDiscriminantClassifier(X,y,scale=FALSE,x_center=FALSE),X_u)[,1] 

  opt_result <- optim(g, fn=loss_iclda, gr=gradient_iclda, 
                      X=X, Y=Y,X_u=X_u, 
                      method="L-BFGS-B", lower=0.0, upper=1.0, 
                      control=list(fnscale=-1))
  
  g <- opt_result$par
  w <- psi(g,X,Y,X_u)
  
  # Return vector of vars as vars
  vars<-vec2vars(w, k, ncol(Y))
  m<-vars$m
  prior<-vars$prior
  sigmas<-vars$sigmas
  if (sup_prior) { prior<-matrix(colMeans(Y),2,1) }
  
  new("ICLinearDiscriminantClassifier", prior=prior, means=m, sigma=sigmas,classnames=classnames,scaling=scaling, modelform=modelform,responsibilities=g)
}

vec2vars<-function(w, k, n_class) {
  prior<-matrix(NA,n_class,1)
  prior[1,] <- w[1]
  prior[2,] <- 1-w[1]
  
  m<-matrix(NA,n_class,k)
  m[1,] <- w[(1+1):(1+k)]
  m[2,] <- w[(1+1+k):(1+2*k)]
  sigmas<-list()
  Sigma <- diag(k) 
  Sigma[upper.tri(Sigma, diag=TRUE)] <- w[(1+2*k+1):length(w)] 
  Sigma <- Sigma + t(Sigma) - diag(diag(Sigma),nrow(Sigma),ncol=ncol(Sigma))
  
  sigmas[[1]] <- Sigma
  sigmas[[2]] <- Sigma
  
  return(list(m=m,prior=prior,sigmas=sigmas))
}

L_sl <- function(w,X,Y) {
  k <- ncol(X)
  
  vars<-vec2vars(w, ncol(X), ncol(Y))
  m<-vars$m
  prior<-vars$prior
  sigmas<-vars$sigmas
  
  ll<-0
  for (c in 1:nrow(m)) {
    sigma<-sigmas[[c]]
    Xc<-X[as.logical(Y[,c]), ,drop=FALSE] #Select all object in class c
    ll<-ll + nrow(Xc) * ( log(prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) #Add the constant part for each row in Xc
    ll<-ll-(1/2)*sum((Xc-(matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))%*%solve(sigma) * (Xc-(matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE]))) #Add the dynamic contribution
  }
  
  return(ll)
}

grad_sl <- function(w, X, Y) {
  vars<-vec2vars(w, ncol(X), ncol(Y))
  m<-vars$m
  prior<-vars$prior
  sigmas<-vars$sigmas
  
  X1<-X[as.logical(Y[,1]), ,drop=FALSE]
  X2<-X[as.logical(Y[,2]), ,drop=FALSE]
  counts<-colSums(Y)
  grad_prior <- counts[1]*(1/prior[1])-counts[2]*(1/(1-prior[1]))
  grad_mean1 <- (colSums(X1)-counts[1]*m[1,]) %*% solve(sigmas[[1]])
  grad_mean2 <- (colSums(X2)-counts[2]*m[2,]) %*% solve(sigmas[[2]])
  grad_sigma <- -0.5*(sum(counts))*solve(sigmas[[1]]) + 0.5 * (solve(sigmas[[1]]) %*% t(X1-matrix(1,counts[1],1) %*% m[1, ,drop=FALSE]) %*% (X1-matrix(1,counts[1],1) %*% m[1, ,drop=FALSE]) %*% solve(sigmas[[1]]) + solve(sigmas[[1]]) %*% t(X2-matrix(1,counts[2],1) %*% m[2, ,drop=FALSE]) %*% (X2-matrix(1,counts[2],1) %*% m[2, ,drop=FALSE]) %*% solve(sigmas[[1]]))
  
  grad_sigma[upper.tri(grad_sigma, diag=FALSE)] <- grad_sigma[upper.tri(grad_sigma, diag=FALSE)]*2 #TODO: check this gradient analytically
  grad_sigma<-grad_sigma[upper.tri(grad_sigma, diag=TRUE)]
  
  return(c(grad_prior,grad_mean1,grad_mean2,as.vector(grad_sigma)))
}

psi <- function(g,X,Y,X_u) {
  g<-matrix(g,length(g),1)
  Y_e<-rbind(Y,cbind(g,1-g))
  X_e<-rbind(X,X_u)
  
  prior<-matrix(colMeans(Y_e),2,1)
  
  #Set priors if not set by user
  if (is.null(prior)) prior<-matrix(colMeans(Y_e),2,1)
  
  #Calculate means for classes
  means<-t((t(X_e) %*% Y_e))/(colSums(Y_e))
  
  sigma <- (t(X_e-matrix(1,nrow(Y_e),1) %*% means[1, ,drop=FALSE]) %*% diag(Y_e[,1]) %*% (X_e-matrix(1,nrow(Y_e),1) %*% means[1, ,drop=FALSE]) + t(X_e-matrix(1,nrow(Y_e),1) %*% means[2, ,drop=FALSE]) %*% diag(Y_e[,2]) %*% (X_e-matrix(1,nrow(Y_e),1) %*% means[2, ,drop=FALSE]))/(nrow(Y_e))
  #     # Supervised Sigma     
  #     means_s<-t((t(X) %*% Y))/(colSums(Y))
  #     sigma <- (t(X-matrix(1,nrow(Y),1) %*% means_s[1, ,drop=FALSE]) %*% diag(Y[,1]) %*% (X-matrix(1,nrow(Y),1) %*% means_s[1, ,drop=FALSE]) + t(X-matrix(1,nrow(Y),1) %*% means_s[2, ,drop=FALSE]) %*% diag(Y[,2]) %*% (X-matrix(1,nrow(Y),1) %*% means_s[2, ,drop=FALSE]))/(nrow(Y))
  
  sigma<-sigma[upper.tri(sigma, diag=TRUE)]
  w <- c(prior[1],as.vector(t(means)),as.vector(sigma))
}

psi_grad <- function(g,X,Y,X_u) {
  k <- ncol(X)
  X1<-X[as.logical(Y[,1]), ,drop=FALSE]
  X2<-X[as.logical(Y[,2]), ,drop=FALSE]
  # counts<-colSums(Y)
  Y_e<-rbind(Y,cbind(g,1-g))
  X_e<-rbind(X,X_u)
  counts_e<-colSums(Y_e)
  m<-t((t(X_e) %*% Y_e))/(colSums(Y_e))
  grad_prior<-matrix(rep(1/(nrow(Y_e)),length(g)),ncol=1)
  
  grad_mean1 <- -((matrix(1,length(g),1) %*% t((t(X_e) %*% Y_e))[1,])-counts_e[1]*X_u)/(counts_e[1]^2)
  grad_mean2 <- ((matrix(1,length(g),1) %*% t((t(X_e) %*% Y_e))[2,])-counts_e[2]*X_u)/(counts_e[2]^2)
  grad_sigma<-matrix(NA,nrow(X_u),k*(k+1)/2)
  for (i in 1:length(g)) {
    gs<- (t(X_u[i,,drop=FALSE]-m[1,]) %*% (X_u[i,,drop=FALSE]-m[1,]) - t(X_u[i,,drop=F]-m[2,]) %*% (X_u[i,,drop=F]-m[2,]))/nrow(Y_e)
    grad_sigma[i,] <- gs[upper.tri(gs, diag=TRUE)]
  }
  return(cbind(grad_prior,grad_mean1,grad_mean2,grad_sigma))
}

loss_iclda <- function(g,X,Y,X_u) {
  w <- psi(g,X,Y,X_u)

  return(L_sl(w,X,Y))
}

gradient_iclda <- function(g,X,Y,X_u) {
  w<-psi(g,X,Y,X_u)
  
  grad<-t(grad_sl(w,X,Y)) %*% t(psi_grad(g,X,Y,X_u))
  return(grad)
}
