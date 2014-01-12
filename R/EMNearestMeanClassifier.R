#' @include NearestMeanClassifier.R
setClass("EMNearestMeanClassifier",
         representation(responsibilities="matrix"),
         prototype(name="Expectation Maximization Nearest Mean Classifier"),
         contains="NearestMeanClassifier")

#' Expectation Maximization Nearest Mean Classifier
EMNearestMeanClassifier <- function(X, y, X_u, method="EM",scale=FALSE, eps=1e-4, ...) {
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
    responsibilities<-posterior(NearestMeanClassifier(X,y),X_u) # Set posterior on the unlabeled objects based an classifier estimated on labeled objects
    iteration <- 0
    while (max(abs(responsibilities-responsibilities_old)) > eps) {
      iteration<- iteration+1
      if (iteration>100) { break }
      prior<-matrix(colMeans(rbind(Y,responsibilities)),2,1)
      
      means<-t((t(Xe) %*% rbind(Y,responsibilities)))/(colSums(rbind(Y,responsibilities)))
      Ye<-rbind(Y,responsibilities)
      sigma<-(sum(Ye[,1] * (Xe-(matrix(1,nrow(Xe),1) %*% means[1,,drop=FALSE]))^2)+sum(Ye[,2] * (Xe-(matrix(1,nrow(Xe),1) %*%means[2,,drop=FALSE]))^2))/(nrow(Xe)*ncol(Xe))
      
      sigma<-diag(ncol(X))*sigma
      sigma<-lapply(1:ncol(Y),function(c){sigma})
      
      g_iteration<-new("NearestMeanClassifier", modelform=modelform, means=means, prior=prior, sigma=sigma,classnames=classnames,scaling=scaling)
#       print(losspart(g_iteration,Xe,Ye))
      
      responsibilities_old <- responsibilities
      responsibilities<-posterior(g_iteration,X_u)
      Ye<-rbind(Y,responsibilities)
#       print(losspart(g_iteration,Xe,Ye))
    }
  }
  new("EMNearestMeanClassifier", modelform=modelform, means=means, prior=prior, sigma=sigma,classnames=classnames,scaling=scaling,responsibilities=responsibilities)
}
