# Nearest Mean using moment constraints. See Loog (2012)

# Formal class definition

setClass("EMNearestMeanClassifier",
         representation(),
         prototype(name="Expectation Maximization Nearest Mean Classifier"),
         contains="NearestMeanClassifier")

# Constructor method: XY
EMNearestMeanClassifierXY <- function(X, y, X_u, method="EM",scale=FALSE, ...) {
  if (scale) {
    library(pls)
    scaling<-stdize(rbind(X,X_u), center = TRUE, scale = TRUE)
    X<-predict(scaling,X)
    X_u<-predict(scaling,X_u)
  } else {scaling=NULL}
  
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="EM") {
    
    
    
  } else if (method=="ml") {
    
  }
  new("EMNearestMeanClassifier", means=means, prior=prior, sigma=sigma,classnames=1:ncol(Y),scaling=scaling)
}

EMNearestMeanClassifier <- function(model, D, method="closedform",prior=NULL,scale=FALSE) {
  list2env(SSLDataFrameToMatrices(model,D,intercept=FALSE),env=environment())
  
  # Fit model
  trained<-EMNearestMeanClassifierXY(X, y, X_u, method=method, prior=prior, scale=scale)
  trained@modelform<-model
  trained@classnames<-classnames
  return(trained)
}