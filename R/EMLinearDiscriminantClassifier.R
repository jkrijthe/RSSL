#' @include LinearDiscriminantClassifier.R
setClass("EMLinearDiscriminantClassifier",
         representation(responsibilities="matrix"),
         prototype(name="Expectation Maximization Linear Discriminant Classifier"),
         contains="LinearDiscriminantClassifier")

#' Semi-Supervised Linear Discriminant Analysis using Expectation Maximization
#' 
#' Expectation Maximization applied to the linear discriminant classifier assuming Gaussian classes with a shared covariance matrix.
#' 
#' Starting from the supervised solution, uses the Expectation Maximization algorithm (see Dempster et al. (1977)) to iteratively update the means and shared covariance of the classes (Maximization step) and updates the responsibilities for the unlabeled objects (Expectation step).
#' 
#' @references Dempster, A., Laird, N. & Rubin, D., 1977. Maximum likelihood from incomplete data via the EM algorithm. Journal of the Royal Statistical Society. Series B, 39(1), pp.1-38.
#' 
#' @param method character; Currently only "EM"
#' @param eps Stopping criterion for the maximinimization
#' @param verbose logical; Controls the verbosity of the output
#' @param max_iter integer; Maximum number of iterations

#' @inheritParams BaseClassifier
#' @family RSSL classifiers
#' 
#' @export
EMLinearDiscriminantClassifier <- function(X, y, X_u, method="EM",scale=FALSE, eps=1e-8, verbose=FALSE, max_iter=100) {
  
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
  if (method=="EM") {
    responsibilities_old<-matrix(0,nrow(X_u),length(classnames)) # Set all posteriors to 0
    g_iteration<-LinearDiscriminantClassifier(X,y)
    responsibilities<-posterior(g_iteration,X_u) # Set posterior on the unlabeled objects based an classifier estimated on labeled objects
    iteration <- 0
    Ye<-rbind(Y,responsibilities)
    logmarginal_old<-Inf
    logmarginal <- losspart(g_iteration,Xe,Ye) #losslogsum(g_iteration,X,Y,X_u,responsibilities)
#     print(logmarginal)
    while (abs(logmarginal-logmarginal_old) > eps) {
      iteration<- iteration+1
      if (iteration>max_iter) { warning("Maximum number of iterations exceeded"); break }
      
      
      prior<-matrix(colMeans(Ye),2,1)
      
      means<-t((t(Xe) %*% Ye))/(colSums(Ye))
      
#       sigma<-(sum(Ye[,1] * t(Xe-(matrix(1,nrow(Xe),1) %*% means[1,,drop=FALSE])) %*% (Xe-(matrix(1,nrow(Xe),1) %*% means[1,,drop=FALSE])))+sum(Ye[,2] * (Xe-(matrix(1,nrow(Xe),1) %*%means[2,,drop=FALSE]))^2))/(nrow(Xe)*ncol(Xe))
      sigma <- (t(Xe-matrix(1,nrow(Ye),1) %*% means[1, ,drop=FALSE]) %*% diag(Ye[,1]) %*% (Xe-matrix(1,nrow(Ye),1) %*% means[1, ,drop=FALSE]) + t(Xe-matrix(1,nrow(Ye),1) %*% means[2, ,drop=FALSE]) %*% diag(Ye[,2]) %*% (Xe-matrix(1,nrow(Ye),1) %*% means[2, ,drop=FALSE]))/(nrow(Ye))
      
      sigma<-lapply(1:ncol(Y),function(c){sigma})
      
      g_iteration<-new("LinearDiscriminantClassifier", modelform=NULL, means=means, prior=prior, sigma=sigma,classnames=classnames,scaling=scaling)
      
      if (verbose) cat("Loss after Parameter update: ",losspart(g_iteration,Xe,Ye),"\n")
      
      responsibilities_old <- responsibilities
      responsibilities<-posterior(g_iteration,X_u)
      Ye<-rbind(Y,responsibilities)
      
      logmarginal_old <- logmarginal
      logmarginal <- losspart(g_iteration,Xe,Ye)
      if (verbose) cat("Loss after Posterior update: ", losspart(g_iteration,Xe,Ye),"\n")
      if (verbose) cat("Log of sum loss: ",losslogsum(g_iteration,X,Y,X_u,responsibilities),"\n")
    }
  }
  
  new("EMLinearDiscriminantClassifier", modelform=modelform, 
      means=means, prior=prior, sigma=sigma,
      classnames=classnames,scaling=scaling,
      responsibilities=responsibilities)
}
