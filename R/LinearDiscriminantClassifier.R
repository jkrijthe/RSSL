#' @include NormalBasedClassifier.R
setClass("LinearDiscriminantClassifier",
         representation(means="matrix",sigma="list",prior="matrix"),
         prototype(name="Linear Discrimant Classifier"),
         contains="NormalBasedClassifier")

#' Linear Discriminant Classifier
#'
#' Implementation of the linear discriminant classifier. Classes are modeled as Gaussians with different means but equal covariance matrices. The optimal covariance matrix and means for the classes are found using maximum likelihood, which, in this case, has a closed form solution.
#'
#' @family RSSL classifiers
#' 
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param method the method to use. Either "closedform" for the fast closed form solution or "ml" for explicit maximum likelihood maximization
#' @param prior A matrix with class prior probabilities. If NULL, this will be estimated from the data
#' @param scale logical; If TRUE, apply a z-transform to the design matrix X before running the regression
#' @param x_center logical; Whether the feature vectors should be centered
#' 
#' @return S4 object of class LeastSquaresClassifier with the following slots:
#' \item{modelform}{weight vector}
#' \item{prior}{the prior probabilities of the classes}
#' \item{mean}{the estimates means of the classes}
#' \item{sigma}{The estimated covariance matrix}
#' \item{classnames}{a vector with the classnames for each of the classes}
#' \item{scaling}{scaling object used to transform new observations}
#' 
#' @export
LinearDiscriminantClassifier<- function(X, y, method="closedform",prior=NULL, scale=FALSE, x_center=FALSE) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,X_u=NULL,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  if (any(table(y)<2)) { stop("Some classes contain fewer than 2 objects.")}
  Y <- model.matrix(~as.factor(y)-1)
  
  if (method=="closedform") {
    
    #Set priors if not set by user
    if (is.null(prior)) prior<-matrix(colMeans(Y),2,1)
    
    #Calculate means for classes
    means<-t((t(X) %*% Y))/(colSums(Y))
    
    #Set sigma to be the average within scatter matrix
    
    sigma.classes<-lapply(1:ncol(Y),function(c,X){cov_ml(X[Y[,c]==1, ,drop=FALSE])},X)
    
    sigma<-sigma.classes[[1]]*prior[1]
    for (i in 2:length(sigma.classes)) {
      sigma<-sigma+sigma.classes[[i]]*prior[i]
    }
      sigma<-lapply(1:ncol(Y),function(c){sigma})
    } else if (method=="ml") {
    
    opt_func<-function(theta, X, y) {
      means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
      sigma<-matrix(theta[((ncol(Y)*ncol(X))+1):length(theta)],ncol(X),ncol(X))
      sigma<-lapply(1:ncol(Y),function(c){sigma})
      
      model<-new("LinearDiscriminantClassifier", prior=prior, means=means, sigma=sigma,classnames=classnames,scaling=scaling)
      loss(model,X,y)
    }
    
    theta<-rep(0.01,3)
    opt_result <- optim(theta, opt_func, gr=NULL, X, y, method="L-BFGS-B", lower=c(-Inf,-Inf,0.000000001),control=list(trace=1,maxit=1000))
    theta<-opt_result$par
    
    means<-matrix(theta[1:(ncol(Y)*ncol(X))],ncol(Y),ncol(X))
    sigma<-theta[(ncol(Y)*ncol(X))+1]
    sigma<-diag(ncol(X))*sigma
    sigma<-lapply(1:ncol(Y),function(c){sigma})
  }
  new("LinearDiscriminantClassifier", modelform=modelform, prior=prior, means=means, sigma=sigma,classnames=classnames,scaling=scaling)
}
