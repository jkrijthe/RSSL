#' @include NearestMeanClassifier.R
setClass("EMNearestMeanClassifier",
         representation(responsibilities="matrix", iterations="numeric"),
         prototype(name="Expectation Maximization Nearest Mean Classifier"),
         contains="NearestMeanClassifier")

#' Semi-Supervised Nearest Mean Classifier using Expectation Maximization
#' 
#' Expectation Maximization applied to the nearest mean classifier assuming Gaussian classes with a spherical covariance matrix.
#' 
#' Starting from the supervised solution, uses the Expectation Maximization algorithm (see Dempster et al. (1977)) to iteratively update the means and shared covariance of the classes (Maximization step) and updates the responsibilities for the unlabeled objects (Expectation step).
#'
#' @references Dempster, A., Laird, N. & Rubin, D., 1977. Maximum likelihood from incomplete data via the EM algorithm. Journal of the Royal Statistical Society. Series B, 39(1), pp.1-38.
#' 
#' @param method character; Currently only "EM"
#' @param scale Should the features be normalized? (default: FALSE)
#' @param eps Stopping criterion for the maximinimization
#' @inheritParams BaseClassifier
#' 
#' @export
EMNearestMeanClassifier <- function(X, y, X_u, method="EM",scale=FALSE, eps=1e-4) {
  
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
    responsibilities_old <- matrix(0,nrow(X_u),length(classnames)) # Set all posteriors to 0
    responsibilities <- posterior(NearestMeanClassifier(X,y),X_u) # Set posterior on the unlabeled objects based an classifier estimated on labeled objects
    iteration <- 0
    while (max(abs(responsibilities-responsibilities_old)) > eps) {
      iteration <- iteration+1
      if (iteration>100) { break }
      prior <- matrix(colMeans(rbind(Y,responsibilities)),2,1)
      
      means <- t((t(Xe) %*% rbind(Y,responsibilities)))/(colSums(rbind(Y,responsibilities)))
      Ye <- rbind(Y,responsibilities)
      sigma <- (sum(Ye[,1] * (Xe-(matrix(1,nrow(Xe),1) %*% means[1,,drop=FALSE]))^2)+sum(Ye[,2] * (Xe-(matrix(1,nrow(Xe),1) %*%means[2,,drop=FALSE]))^2))/(nrow(Xe)*ncol(Xe))
      
      sigma <- diag(ncol(X))*sigma
      sigma <- lapply(1:ncol(Y),function(c){sigma})
      
      g_iteration <- new("NearestMeanClassifier", modelform=NULL, means=means, prior=prior, sigma=sigma,classnames=classnames,scaling=scaling)
#       print(losspart(g_iteration,Xe,Ye))
      
      responsibilities_old <- responsibilities
      responsibilities <- posterior(g_iteration,X_u)
      Ye <- rbind(Y,responsibilities)
#       print(losspart(g_iteration,Xe,Ye))
    }
  }
  new("EMNearestMeanClassifier", 
      modelform=modelform, classnames=classnames,
      means=means, prior=prior, sigma=sigma,
      scaling=scaling,
      responsibilities=responsibilities,iterations=iteration)
}
