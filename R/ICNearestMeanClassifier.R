#' @include NearestMeanClassifier.R
setClass("ICNearestMeanClassifier",
         representation(),
         prototype(name="Implicitly Constrained Nearest Mean Classifier"),
         contains="NearestMeanClassifier")

#' Moment Constrained Nearest Mean Classifier (Semi-Supervised)
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
ICNM <- ICNearestMeanClassifier <- function(X, y, X_u, method="closedform",scale=FALSE, ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  
  L_sl <- function(w,X,Y,lambda) {
      k<-ncol(X) # Number of features

      prior<-matrix(NA,length(classnames),1)
      prior[1,] <- w[1]
      prior[2,] <- 1-w[1]
      m<-matrix(NA,length(classnames),k)
      m[1,] <- w[(1+1):(1+k)]
      m[2,] <- w[(1+1+k):(1+2*k)]
      sigmas<-list()
      Sigma <- diag(k) 
      Sigma[upper.tri(Sigma, diag=TRUE)] <- w[(1+2*k+1):length(w)] 
      Sigma <- Sigma + t(Sigma) - diag(diag(Sigma))

      sigmas[[1]] <- Sigma
      sigmas[[2]] <- Sigma

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
      k<-ncol(X)
      prior<-matrix(NA,length(classnames),1)
      prior[1,] <- w[1]
      prior[2,] <- 1-w[1]
      m<-matrix(NA,length(classnames),k)
      m[1,] <- w[(1+1):(1+k)]
      m[2,] <- w[(1+1+k):(1+2*k)]
      sigmas<-list()

      Sigma <- diag(k) 
      Sigma[upper.tri(Sigma, diag=TRUE)] <- w[(1+2*k+1):length(w)] 
      Sigma <- Sigma + t(Sigma) - diag(diag(Sigma))

      sigmas[[1]] <- Sigma
      sigmas[[2]] <- Sigma

      X1<-X[as.logical(Y[,1]), ,drop=FALSE]
      X2<-X[as.logical(Y[,2]), ,drop=FALSE]
      counts<-colSums(Y)
      grad_prior <- counts[1]*(1/prior[1])-counts[2]*(1/(1-prior[1]))
      grad_mean1 <- (colSums(X1)-counts[1]*m[1,]) %*% solve(sigmas[[1]])
      grad_mean2 <- (colSums(X2)-counts[2]*m[2,]) %*% solve(sigmas[[2]])
      grad_sigma <- -0.5*(sum(counts))*solve(sigmas[[1]]) + 0.5 * (solve(sigmas[[1]]) %*% t(X1-matrix(1,counts[1],1) %*% m[1, ,drop=FALSE]) %*% (X1-matrix(1,counts[1],1) %*% m[1, ,drop=FALSE]) %*% solve(sigmas[[1]]) + solve(sigmas[[1]]) %*% t(X2-matrix(1,counts[2],1) %*% m[2, ,drop=FALSE]) %*% (X2-matrix(1,counts[2],1) %*% m[2, ,drop=FALSE]) %*% solve(sigmas[[1]]))
      grad_sigma<-grad_sigma[upper.tri(grad_sigma, diag=TRUE)]

      return(c(grad_prior,grad_mean1,grad_mean2,as.vector(grad_sigma)))
    }

  
  L_ssl <- function(w,g, X, y, X_u) {
    g<-as.vector(g)
    # g<-rep(0,nrow(X_u))
    loss <- L_sl(w,X,y) + sum(g * log(matrix(1,nrow(X_u),1)+exp(-1 * (X_u %*% w))) + (1-g) * log(matrix(1,nrow(X_u),1)+exp((X_u %*% w))))
    return(as.numeric(loss))
  }
  
  grad_ssl <- function(w,g, X, y, X_u) {
    g<-as.vector(g)
    # g<-rep(0,nrow(X_u))
    grad_sl(w,X,y) + t(X_u) %*% (g*(-1 * (exp(-1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(-1 * (X_u %*% w))))) + (1-g) * (1 * (exp(1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(1 * (X_u %*% w)))))) 
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

    sigma<-sigma[upper.tri(sigma, diag=TRUE)]
    w <- c(prior[1],as.vector(t(means)),as.vector(sigma))
  }

  psi_grad<-function(g,X,Y,X_u) {
    k<-ncol(X)
    
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
      gs<- (t(X_u[i,,drop=F]-m[1,]) %*% (X_u[i,,drop=F]-m[1,]) - t(X_u[i,,drop=F]-m[2,]) %*% (X_u[i,,drop=F]-m[2,]))/nrow(Y_e)
      grad_sigma[i,] <-gs[upper.tri(gs, diag=TRUE)]
    }
    return(cbind(grad_prior,grad_mean1,grad_mean2,grad_sigma))

  }

  loss<-function(g,X,Y,X_u) {
    w<-psi(g,X,Y,X_u)
    # if (verbose) cat("Loss: ", L_sl(w,X,y),"\n")
    L_sl(w,X,Y,X_u)
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
    # psi_grad(g,X,Y,X_u)[490,]
    # grad(function(u) {psi(u,X,Y,X_u)[6]},g)[490,]
    # grad(function(u) {L_sl(u,X,Y)},w)
    # grad_sl(w,X,Y)
    # print(cbind(t(grad),t(t(true))))

    grad<-t(grad_sl(w,X,Y)) %*% t(psi_grad(g,X,Y,X_u))
    return(grad)
  }
  
 
  g <- rep(0.5,nrow(X_u))


  opt_result <- optim(g, fn=loss, gr=gradient, X=X, Y=Y,X_u=X_u, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=-1))
  # g<-as.numeric(opt_result[1,1:length(g)])
  g<-opt_result$par
  w<-psi(g,X,y,X_u)

  k<-ncol(X)
  prior<-matrix(NA,length(classnames),1)
      prior[1,] <- w[1]
      prior[2,] <- 1-w[1]
      means<-matrix(NA,length(classnames),k)
      means[1,] <- w[(1+1):(1+k)]
      means[2,] <- w[(1+1+k):(1+2*k)]
      sigmas<-list()
      Sigma <- diag(k) 
      Sigma[upper.tri(Sigma, diag=TRUE)] <- w[(1+2*k+1):length(w)] 
      Sigma <- Sigma + t(Sigma) - diag(diag(Sigma))

      sigmas[[1]] <- Sigma
      sigmas[[2]] <- Sigma
  new("ICNearestMeanClassifier", modelform=modelform, means=means, prior=prior, sigma=sigma,classnames=classnames,scaling=scaling, modelform=modelform,responsibilities=g)
}
