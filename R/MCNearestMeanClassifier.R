#' @include NearestMeanClassifier.R
setClass("MCNearestMeanClassifier",
         representation(),
         prototype(name="Moment Constrained Nearest Mean Classifier through ad hoc mean shifting"),
         contains="NearestMeanClassifier")

#' Moment Constrained Semi-supervised Nearest Mean Classifier
#'
#' To fit a true nearest mean classifier, set prior to equal class priors. Based on (Loog 2010).
#'
#' @references Loog, M., 2010. Constrained Parameter Estimation for Semi-Supervised Learning: The Case of the Nearest Mean Classifier. In Proceedings of the 2010 European Conference on Machine learning and Knowledge Discovery in Databases. pp. 291-304.
#'
#' @param method Estimation procedure: c("closedform","ml")
#' @inheritParams BaseClassifier
#' @return S4  object; a list consisting of
#' \item{means}{the approximation of piel}
#' \item{prior}{the number of trials}
#' \item{sigma}{the number of hits}
#' @family RSSL classifiers
#' 
#' @export
MCNearestMeanClassifier <- function(X, y, X_u, method="closedform",scale=FALSE,x_center=FALSE) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
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
