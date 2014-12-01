#' @include LinearDiscriminantClassifier.R
setClass("CPLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix",responsibilities="ANY"),
         prototype(name="Contrastive Pessimistic Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' @title Constrastive Pessimistic Semi-Supervised Linear Discriminant Classifier
#'
#' @description Marco Loog's CPLDA classifier
#'
#' @param X design matrix of the labeled objects
#' @param y vector with labels
#' @param X_u design matrix of the labeled objects
#' @param prior <what param does>
#' @param scale Should the features be normalized? (default: FALSE)
#' @param init not currently used
#' @param max_iterations Maximimum number of iterations for the maximinimization
#' @param alpha numeric; Learning rate in the minimax solving
#' @param eps Stopping criterion for the maximinimization
#' @param x_center logical; Whether the feature vectors should be centered
#' @param ... Additional Parameters, Not used
#' @export
CPLinearDiscriminantClassifier <- function(X, y, X_u, prior=NULL, scale=FALSE, init=NULL, max_iterations=1000, alpha=1, eps=1e-4, x_center=FALSE,  ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  k<-ncol(X) # Number of features
  
  X_e <- rbind(X,X_u)
  vec2vars<-function(w) {
    prior<-matrix(NA,length(classnames),1)
    prior[1,] <- w[1]
    prior[2,] <- 1-w[1]
    
    m<-matrix(NA,length(classnames),k)
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
    vars<-vec2vars(w)
    m<-vars$m
    prior<-vars$prior
    sigmas<-vars$sigmas
    ll <- matrix(0,nrow=nrow(X),ncol=1)
    
    for (c in 1:nrow(m)) {
      sigma<-sigmas[[c]]
      Xc<-X[as.logical(Y[,c]), ,drop=FALSE] #Select all object in class c
      if (nrow(Xc)==0) { next }
      ll<- ll + ( log(prior[c,])-(k/2)*log(2*pi)-(1/2)*log(det(sigma)) ) #Add the constant part for each row in Xc
      ll<- ll + rowSums(-(1/2)*(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma) * (Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])) #Add the dynamic contribution
    }
    return(ll)
  }
  
  
  psi<-function(g,X,y,X_u) {
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
  
  objective<-function(g,w,X,Y,X_u) {
    vars<-vec2vars(w)
    m<-vars$m
    prior<-vars$prior
    sigmas<-vars$sigmas
    g_semi<-new("CPLinearDiscriminantClassifier", prior=prior, means=m, sigma=sigmas,classnames=classnames,scaling=NULL, modelform=modelform,responsibilities=g)
    
    G<-cbind(g,1-g)
    (losspart(g_semi,X_e,rbind(Y,G))-losspart(g_sup,X_e,rbind(Y,G)))/nrow(X_e)
  }
  
  gradient<-function(g,w,X,Y,X_u) {

    w_semi<-w
    # Testing Code
#     print(cbind(-grad(objective,g,w=w_semi,X=X,Y=Y,X_u=X_u),L_sl(w_semi,X_u,cbind(rep(1,nrow(X_u)),rep(0,nrow(X_u)))) - 
#                   L_sl(w_semi,X_u,cbind(rep(0,nrow(X_u)),rep(1,nrow(X_u)))) -
#                   L_sl(w_sup,X_u,cbind(rep(1,nrow(X_u)),rep(0,nrow(X_u))))+L_sl(w_sup,X_u,cbind(rep(0,nrow(X_u)),rep(1,nrow(X_u))))))
#     browser()

    grad<-L_sl(w_semi,X_u,cbind(rep(1,nrow(X_u)),rep(0,nrow(X_u)))) - 
      L_sl(w_semi,X_u,cbind(rep(0,nrow(X_u)),rep(1,nrow(X_u)))) -
      L_sl(w_sup,X_u,cbind(rep(1,nrow(X_u)),rep(0,nrow(X_u))))+L_sl(w_sup,X_u,cbind(rep(0,nrow(X_u)),rep(1,nrow(X_u))))
    grad/nrow(X_e)
  }
  
  # Initialization
  g <-rep(0.5,nrow(X_u))
  g_sup <-LinearDiscriminantClassifier(X,y,x_center=FALSE)
  
  sigma_sup <-g_sup@sigma[[1]][upper.tri(g_sup@sigma[[1]], diag=TRUE)]
  w_sup <- c(g_sup@prior[1],as.vector(t(g_sup@means)),as.vector(sigma_sup))
  
  w<-psi(g,X,y,X_u)
  vars<-vec2vars(w)
  m<-vars$m
  prior<-vars$prior
  sigmas<-vars$sigmas
  
  objective_value_old <- objective(g,w,X,Y,X_u)
#   print(objective_value_old)
  for (i in 1:max_iterations) {
    stepsize<-alpha*(1/max_iterations)
    # For given labeling, find optimal parameters
    w<-psi(g,X,y,X_u)
#     print(objective(g,w,X,Y,X_u))
    # For a given parameter vector make a step
    g<- g - alpha*gradient(g,w,X,Y,X_u)
    
    # Simplified projection
    g<-sapply(g,function(x) { max(min(1,x),0)})
    
    # Check if we should stop
    objective_value <- objective(g,w,X,Y,X_u)
#     print(objective_value)
    if (abs(objective_value_old-objective_value)<eps) {
      break
    } else {
      objective_value_old<-objective_value
    }
  }
  print(i)
  w<-psi(g,X,y,X_u)
  
  # Return vector of vars as vars
  vars<-vec2vars(w)
  m<-vars$m
  prior<-vars$prior
  sigmas<-vars$sigmas
  
  new("CPLinearDiscriminantClassifier", prior=prior, means=m, sigma=sigmas,classnames=classnames,scaling=scaling, modelform=modelform,responsibilities=g)
}
