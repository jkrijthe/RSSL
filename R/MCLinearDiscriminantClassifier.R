# Nearest Mean using moment constraints. See Loog (2012)

# Formal class definition

setClass("MCLinearDiscriminantClassifier",
         representation(),
         prototype(name="Moment Constrained Linear Discriminant Classifier through ad hoc mean shifting"),
         contains="LinearDiscriminantClassifier")

# Constructor method: XY
MCLinearDiscriminantClassifierXY <- function(X, Y, Xu, method="closedform",scale=TRUE) {
  if (scale) {
    library(pls)
    scaling<-stdize(rbind(X,Xu), center = TRUE, scale = TRUE)
    X<-predict(scaling,X)
    Xu<-predict(scaling,Xu)
  } else {scaling=NULL}
  
  if (method=="closedform") {
    browser()
    means<-t((t(X) %*% Y))/(colSums(Y))
    sigma<-mean((X-(Y %*% means))^2)
    
    m_t <- colMeans(rbind(X,Xu)) # Overal mean of all data
    m_t_l <- colMeans(X) # Overal mean of labeled data
    
    means <- means + matrix(1,nrow(means),1) %*% (m_t-m_t_l) # Apply Loog's mean correction
    
    #Update sigma or not?
    
  } else if (method=="ml") {
    
  }
  new("MCLinearDiscriminantClassifier", means=means, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

MCLinearDiscriminantClassifier <- function(model, D, method="closedform",scale=TRUE) {
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),env=environment())
  
  # Fit model
  trained<-MCLinearDiscriminantClassifierXY(X, Y, X_u, method=method,scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}