# #' @include NearestMeanClassifier.R
# setClass("EMNearestMeanClassifier",
#          representation(),
#          prototype(name="Expectation Maximization Nearest Mean Classifier"),
#          contains="NearestMeanClassifier")

# #' Expectation Maximization Nearest Mean Classifier
EMNearestMeanClassifier <- function(X, y, X_u, method="closedform",scale=FALSE, ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    while (norm(responsibilities-responsibilities_old,"F")<eps) {
      
    }
    
    prior<-matrix(colMeans(Y),2,1)
    means<-t((t(X) %*% Y))/(colSums(Y))
    sigma<-mean((X-(Y %*% means))^2)
    
    m_t <- colMeans(rbind(X,X_u)) # Overal mean of all data
    m_t_l <- colMeans(X) # Overal mean of labeled data
    
    means <- means + matrix(1,nrow(means),1) %*% (m_t-m_t_l) # Apply Loog's mean correction
    
    #Update sigma or not
    sigma<-mean((X-(Y %*% means))^2)
    
    sigma<-diag(ncol(X))*sigma
    
    sigma<-lapply(1:ncol(Y),function(c){sigma})
    
  } else if (method=="ml") {
    
  }
  new("EMNearestMeanClassifier", modelform=modelform, means=means, prior=prior, sigma=sigma,classnames=classnames,scaling=scaling,responsibilities=responsibilities)
}
