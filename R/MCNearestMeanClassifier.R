#' @include NearestMeanClassifier.R
setClass("MCNearestMeanClassifier",
         representation(),
         prototype(name="Moment Constrained Nearest Mean Classifier through ad hoc mean shifting"),
         contains="NearestMeanClassifier")

#' Moment Constrained Semi-supervised Nearest Mean Classifier
#'
#' Update the means based on the moment constraints as defined in Loog (2010). The means estimated using the labeled data are updated by making sure their weighted mean corresponds to the overall mean on all (labeled and unlabeled) data. Optionally, the estimated variance of the classes can be re-estimated after this update is applied by setting update_sigma to \code{TRUE}. To get the true nearest mean classifier, rather than estimate the class priors, set them to equal priors using, for instance \code{prior=matrix(0.5,2)}.
#'
#' @references Loog, M., 2010. Constrained Parameter Estimation for Semi-Supervised Learning: The Case of the Nearest Mean Classifier. In Proceedings of the 2010 European Conference on Machine learning and Knowledge Discovery in Databases. pp. 291-304.
#' 
#' @family RSSL classifiers
#' 
#' @param update_sigma logical; Whether the estimate of the variance should be updated after the means have been updated using the unlabeled data
#' @param prior matrix; Class priors for the classes
#' @inheritParams BaseClassifier
#' 
#' @export
MCNearestMeanClassifier <- function(X, y, X_u, update_sigma=FALSE, prior=NULL, x_center=FALSE, scale=FALSE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- ModelVariables$X
  X_u <- ModelVariables$X_u
  Y <- ModelVariables$Y
  
  # Find initial prior, means and variance
  if (is.null(prior)) { prior <- matrix(colMeans(Y),2,1) }
  means<-t((t(X) %*% Y))/(colSums(Y))
  sigma<-mean((X-(Y %*% means))^2)
  
  # Update means based on unlabeled data
  m_t <- colMeans(rbind(X,X_u)) # Overal mean of all data
  m_t_l <- colMeans(X) # Overal mean of labeled data
  
  means <- means + matrix(1,nrow(means),1) %*% (m_t-m_t_l) # Apply Loog's mean correction
  if (update_sigma) { sigma<-mean((X-(Y %*% means))^2) } # Potentially update variance
  
  sigma<-diag(ncol(X))*sigma
  sigma<-lapply(1:ncol(Y),function(c){sigma})
    
  
  new("MCNearestMeanClassifier",
      means=means, prior=prior, 
      sigma=sigma,
      modelform=ModelVariables$modelform,
      classnames=ModelVariables$classnames,
      scaling=ModelVariables$scaling
  )
}
