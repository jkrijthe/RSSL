#' @include LinearDiscriminantClassifier.R
setClass("ICLinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix",responsibilities="ANY"),
         prototype(name="Implicitly Constrained Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Implicitly Constrained Semi-supervised Linear Discriminant Classifier
#'
#' Semi-supervised version of Linear Discriminant Analysis using implicit constraints as described in (Krijthe & Loog 2014)
#' This method finds the labeling of the unlabeled objects, whose resulting LDA solution gives the highest log-likelihood when evaluated on the labeled objects only.
#'
#' @references Krijthe, J.H. & Loog, M., 2014. Implicitly Constrained Semi-Supervised Linear Discriminant Analysis. In International Conference on Pattern Recognition. Stockholm, pp. 3762-3767.
#'
#' @param X design matrix of the labeled objects
#' @param y vector with labels
#' @param X_u design matrix of the labeled objects
#' @param prior set a fixed class prior
#' @param scale logical; Should the features be normalized? (default: FALSE)
#' @param init not currently used
#' @param sup_prior logical; use the prior estimates based only on the labeled data, not the imputed labels (default: FALSE)
#' @param factr change the precision requirement passed to the optim function (default: 1e7)
#' @param x_center logical; Whether the data should be centered
#' @param ... Additional Parameters, Not used
#' 
#' @export
ICLinearDiscriminantClassifier <- function(X, y, X_u, prior=NULL, scale=FALSE,init=NULL, sup_prior=FALSE, factr=1e7,  x_center=FALSE,...) {
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
    vars<-vec2vars(w)
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
    
    # Numerical gradient: 
    # grad(L_sl,x=w,X=X,Y=Y)
    # optim(c(0.5,0,0,0,0,1,0,1),L_sl,X=X,Y=Y,control=list(fnscale=-1))
    grad_sigma[upper.tri(grad_sigma, diag=FALSE)] <- grad_sigma[upper.tri(grad_sigma, diag=FALSE)]*2 #TODO: check this gradient analytically
    grad_sigma<-grad_sigma[upper.tri(grad_sigma, diag=TRUE)]
  
    return(c(grad_prior,grad_mean1,grad_mean2,as.vector(grad_sigma)))
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

    #Set sigma to be the average within scatter matrix
    # sigma.classes<-lapply(1:ncol(Y),function(c,X){cov(X[Y[,c]==1,])},X)
    # sigma<-sigma.classes[[1]]*prior[1]
    # for (i in 2:length(sigma.classes)) {
    #   sigma<-sigma+sigma.classes[[i]]*prior[i]
    # }

    #TODO: this is the biased estimate of the covariance matrix!
    
    sigma <- (t(X_e-matrix(1,nrow(Y_e),1) %*% means[1, ,drop=FALSE]) %*% diag(Y_e[,1]) %*% (X_e-matrix(1,nrow(Y_e),1) %*% means[1, ,drop=FALSE]) + t(X_e-matrix(1,nrow(Y_e),1) %*% means[2, ,drop=FALSE]) %*% diag(Y_e[,2]) %*% (X_e-matrix(1,nrow(Y_e),1) %*% means[2, ,drop=FALSE]))/(nrow(Y_e))
#     # Supervised Sigma     
#     means_s<-t((t(X) %*% Y))/(colSums(Y))
#     sigma <- (t(X-matrix(1,nrow(Y),1) %*% means_s[1, ,drop=FALSE]) %*% diag(Y[,1]) %*% (X-matrix(1,nrow(Y),1) %*% means_s[1, ,drop=FALSE]) + t(X-matrix(1,nrow(Y),1) %*% means_s[2, ,drop=FALSE]) %*% diag(Y[,2]) %*% (X-matrix(1,nrow(Y),1) %*% means_s[2, ,drop=FALSE]))/(nrow(Y))

    sigma<-sigma[upper.tri(sigma, diag=TRUE)]
    w <- c(prior[1],as.vector(t(means)),as.vector(sigma))
  }

  psi_grad<-function(g,X,Y,X_u) {

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
      grad_sigma[i,] <-gs[upper.tri(gs, diag=TRUE)]
    }
    return(cbind(grad_prior,grad_mean1,grad_mean2,grad_sigma))
  }

  loss<-function(g,X,Y,X_u) {
    w<-psi(g,X,Y,X_u)
    # if (verbose) cat("Loss: ", L_sl(w,X,y),"\n")
#     print(L_sl(w,X,Y))
#     if (is.infinite(L_sl(w,X,Y))) { browser()}
    return(L_sl(w,X,Y))
  }
  
  gradient <- function(g,X,Y,X_u) {
    w<-psi(g,X,Y,X_u)

    ## For testing purposes
    # if (verbose) cat("Gradient norm: ", c(rcond(-Dw(w,g,X,y,X_u)),norm(Dg(w,X,y,X_u),"F")),"\n")
    # true<-grad(function(u) {loss(u,X,y,X_u)},g)
    # print(sum(abs(grad-true)))
    # print(grad)
    # print(true)
    # browser()
    # psi_grad(g,X,Y,X_u)
    # grad(function(u) {psi(u,X,Y,X_u)[6]},g)
    # grad(function(u) {L_sl(u,X,Y)},w)
    # grad_sl(w,X,Y)
    # print(cbind(t(grad),t(t(true))))
    
#     print(w[1:4])
#     print(L_sl(w,X,Y))
    grad<-t(grad_sl(w,X,Y)) %*% t(psi_grad(g,X,Y,X_u))
    return(grad)
  }
  
#   g <- runif(nrow(X_u))
#   g <- rep(1,nrow(X_u))
  g <- posterior(LinearDiscriminantClassifier(X,y),X_u)[,1] # Initialize responsibilities using supervised posterior

  opt_result <- optim(g, fn=loss, gr=gradient, X=X, Y=Y,X_u=X_u, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=-1))
  # g<-as.numeric(opt_result[1,1:length(g)])
  g<-opt_result$par
  w<-psi(g,X,y,X_u)
  
  # Return vector of vars as vars
  vars<-vec2vars(w)
  m<-vars$m
  prior<-vars$prior
  sigmas<-vars$sigmas
  if (sup_prior) { prior<-matrix(colMeans(Y),2,1) }
#   print(L_sl(w,X,Y))
  
  new("ICLinearDiscriminantClassifier", prior=prior, means=m, sigma=sigmas,classnames=classnames,scaling=scaling, modelform=modelform,responsibilities=g)
}
