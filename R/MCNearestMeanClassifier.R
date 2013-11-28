#' @include NearestMeanClassifier.R
setClass("MCNearestMeanClassifier",
         representation(),
         prototype(name="Moment Constrained Nearest Mean Classifier through ad hoc mean shifting"),
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
MCNearestMeanClassifier <- function(X, y, X_u, method="closedform",scale=FALSE, ...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
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
  new("MCNearestMeanClassifier", modelform=modelform, means=means, prior=prior, sigma=sigma,classnames=classnames,scaling=scaling)
}
