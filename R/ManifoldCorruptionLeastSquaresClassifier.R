#' Least Squares Classifier applying gaussian corruption over the manifold
#' @export
ManifoldCorruptionLeastSquaresClassifier <- function(X, y, X_u, lambda=0, adjacency_kernel=vanilladot(), x_center=TRUE, scale=TRUE, y_scale=TRUE,k=10,sigma=1, samples=100,method="marginalize") {
  
  stopifnot(require(kernlab))
  
  ## Preprocessing to correct datastructures and scaling  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  scaling <- ModelVariables$scaling
  classnames <- ModelVariables$classnames
  modelform <- ModelVariables$modelform
  Y <- ModelVariables$Y
  
  ## Start Implementation
  n<-nrow(X)
  m<-ncol(X)
  
  Xtrain<-NULL
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  if (y_scale) {
    y_scale <- colMeans(Y)
  } else {
    y_scale <- rep(0,ncol(Y))
  }
  
  Y <- sweep(Y,2,y_scale) # Possibly center the numeric Y labels
  
#   browser()
#   Xtrain <- rbind(X, X_u)
#   t_kpca <- kpca(Xtrain,kernel=rbfdot(100))
  
  Xtrain <- rbind(X, X_u)
  if (FALSE) {
    W <- kernelMatrix(adjacency_kernel,Xtrain,Xtrain)
    L <- diag(rowSums(W)) - W
    Xall <- matrix(NA,0,2)
    yall <- c()
    for (r in 1:nrow(X)) {
      Xnew <- t(X[r,]+matrix(rnorm(200),2,100)*0.4)
      
      Wnew <- kernelMatrix(adjacency_kernel,rbind(Xtrain,Xnew),rbind(Xtrain,Xnew))
      L <- diag(rowSums(Wnew)) - Wnew
      
      Wnew <- solve(diag(nrow(Wnew)) - L)
      idx <- sample(1:(ncol(Wnew)),100,prob = colSums(Wnew),replace=TRUE)
      idx <- sample(1:100,100,prob = abs(Wnew[r,2001:2100]),replace=TRUE)
      
      Xall<-rbind(Xall,Xnew[idx,])
      yall<-c(yall,rep(Y[r],100))
    }
  } else if (method=="sample") {
    Xall <- matrix(NA,0,2)
    yall <- c()
    for (r in 1:nrow(X)) {
      library(e1071)
      closeX <- Xtrain[order(sqrt(colSums((t(Xtrain)-X[r,])^2)))[1:(2*k)],]
      distmat<- kernelMatrix(adjacency_kernel,rbind(X[r,],closeX),rbind(X[r,],closeX))
      #distmat <- dist(rbind(X[r,],closeX))
      #pat<-allShortestPaths(distmat)
      pat<-list()
      pat$length<-distmat
      #Xlocal <- Xtrain[order(sqrt(colSums((t(Xtrain)-X[r,])^2)))[1:k],]
      #Xlocal <- Xtrain[sqrt(colSums((t(Xtrain)-X[r,])^2))<0.3,]
      Xlocal <- closeX[order(pat$length[1,-1])[1:k],]
      Xnew<-t(t(chol(cov(Xlocal))) %*% matrix(rnorm(samples*ncol(Xlocal)),ncol(Xlocal),samples)*sigma + colMeans(Xlocal))
      Xall<-rbind(Xall,Xnew)
      yall<-c(yall,rep(Y[r],samples))
    }
    
    # Plot generated samples
      df_corrupted <- data.frame(Xall,label=factor(yall))
      df_unlabeled <- data.frame(X_u)
      df_labeled <- data.frame(X,label=y) 
      
      library(LiblineaR)
      t_svm <- LiblineaR(X,y,type=1)
      t_svmcorr <- LiblineaR(Xall,factor(yall),type=1)
      
      p<- ggplot(data=df_unlabeled,aes(x=X1,y=X2)) + 
        geom_point(size=2,color="grey") + 
        geom_point(aes(x=X1,y=X2,color=label),data=df_corrupted,alpha=0.3,size=5) + scale_color_discrete(guide=FALSE) +
        geom_point(aes(x=X1,y=X2,color=label,shape=label),data=df_labeled,size=8) +
        coord_equal() +
        geom_abline(intercept=-t_svm$W[3]/t_svm$W[2],slope=-t_svm$W[1]/t_svm$W[2],linetype=1) +
        geom_abline(intercept=-t_svmcorr$W[3]/t_svmcorr$W[2],slope=-t_svmcorr$W[1]/t_svmcorr$W[2],linetype=2)
      print(p)
    
  } else {
      M <- matrix(NA,nrow(X),ncol(X))
      Sigma <- diag(ncol(X))*lambda #matrix(0,ncol(X),ncol(X))
      for (r in 1:nrow(X)) {
        closeX <- Xtrain[order(sqrt(colSums((t(Xtrain)-X[r,])^2)))[1:(k*(nrow(Xtrain)))],]
        #distmat<- kernelMatrix(adjacency_kernel,rbind(X[r,],closeX),rbind(X[r,],closeX))
        #pat <- list()
        #pat$length<-distmat
        Xlocal <- closeX #[order(pat$length[1,-1])[1:k],]
        M[r,] <- colMeans(Xlocal)
        Sigma <- Sigma + cov(Xlocal)*sigma
      }
      theta <- solve(t(M) %*%M + Sigma, t(M) %*% Y)
      
      new("LeastSquaresClassifier",
          classnames=classnames,
          modelform=modelform,
          theta=theta,
          scaling=scaling,
          intercept=FALSE,
          y_scale=y_scale
      )
    }
}
