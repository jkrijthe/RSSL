#' @include NearestMeanClassifier.R
setClass("ICNearestMeanClassifier",
         representation(responsibilities="ANY"),
         prototype(name="Implicitly Constrained Nearest Mean Classifier"),
         contains="NearestMeanClassifier")

#' Implicitly Constrained Nearest Mean Classifier (Semi-Supervised)
#'
#' To fit a true nearest mean classifier, set prior to equal class priors. Based on Loog (2010)
#'
#' @usage MCNearestMeanClassifier(X, y, X_u, method="closedform", scale=FALSE, ...)
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector with class assignments
#' @param X_u Design matrix of the unlabeled objects, intercept term is added within the function
#' @param method Estimation procedure: c("closedform","ml")
#' @param scale Whether the features should be scaled (default: FALSE)
#' @param ... additional arguments
#' @return S4  object; a list consisting of
#' \item{means}{the approximation of piel}
#' \item{prior}{the number of trials}
#' \item{sigma}{the number of hits}
#' @export
ICNearestMeanClassifier <- function(X, y, X_u, method="closedform",scale=FALSE, ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE)
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
    Sigma <- diag(k) * w[length(w)]
    
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
      ll<-ll+sum(-(1/2)*(Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])%*%solve(sigma) * (Xc-matrix(1,nrow(Xc),1) %*% m[c, ,drop=FALSE])) #Add the dynamic contribution
    }
    return(ll)
  }
  
  grad_sl <- function(w, X, Y,lambda) {
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
    grad_sigma <- 0
    
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
    
    sigma <- 1 #(t(X_e-matrix(1,nrow(Y_e),1) %*% means[1, ,drop=FALSE]) %*% diag(Y_e[,1]) %*% (X_e-matrix(1,nrow(Y_e),1) %*% means[1, ,drop=FALSE]) + t(X_e-matrix(1,nrow(Y_e),1) %*% means[2, ,drop=FALSE]) %*% diag(Y_e[,2]) %*% (X_e-matrix(1,nrow(Y_e),1) %*% means[2, ,drop=FALSE]))/(nrow(Y_e))
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
    grad_sigma <- 0
    return(cbind(grad_prior,grad_mean1,grad_mean2,grad_sigma))
  }
  
  loss<-function(g,X,Y,X_u) {
    w<-psi(g,X,Y,X_u)
    # if (verbose) cat("Loss: ", L_sl(w,X,y),"\n")
    #     print(L_sl(w,X,Y))
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
  
  g <- runif(nrow(X_u))
#   g <- rep(1,nrow(X_u))
#   g<- 1-posterior(LinearDiscriminantClassifier(X,y),X_u)
  
  opt_result <- optim(g, fn=loss, gr=gradient, X=X, Y=Y,X_u=X_u, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=-1))
  # g<-as.numeric(opt_result[1,1:length(g)])
  g<-opt_result$par
  w<-psi(g,X,y,X_u)
  
  # Return vector of vars as vars
  vars<-vec2vars(w)
  m<-vars$m
  prior<-vars$prior
  sigmas<-vars$sigmas
  
  #   print(L_sl(w,X,Y))
  new("ICNearestMeanClassifier", modelform=modelform, means=m, prior=prior, sigma=sigmas,classnames=classnames,scaling=scaling,responsibilities=g)
}
