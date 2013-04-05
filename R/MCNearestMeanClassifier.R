# Nearest Mean using moment constraints. See Loog (2012)

# Formal class definition

setClass("MCNearestMeanClassifier",
         representation(),
         prototype(name="Moment Constrained Nearest Mean Classifier through ad hoc mean shifting"),
         contains="NearestMeanClassifier")

# Constructor method: XY
MCNearestMeanClassifierXY <- function(X, y, X_u, method="closedform",scale=FALSE, ...) {
  if (scale) {
    library(pls)
    scaling<-stdize(rbind(X,X_u), center = TRUE, scale = TRUE)
    X<-predict(scaling,X)
    X_u<-predict(scaling,X_u)
  } else {scaling=NULL}
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
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
  new("MCNearestMeanClassifier", means=means, prior=prior, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

MCNearestMeanClassifier <- function(model, D, method="closedform",prior=NULL,scale=FALSE) {
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),env=environment())
  
  # Fit model
  trained<-MCNearestMeanClassifierXY(X, y, X_u, method=method, prior=prior, scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}