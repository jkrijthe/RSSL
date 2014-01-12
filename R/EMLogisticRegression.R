#' @include LogisticRegression.R
#' @export
#' EMLogisticRegression class
setClass("EMLogisticRegression",
         representation(theta="numeric"),
         prototype(name="Expectation Maximization Logistic Regression"), 
         contains="LogisticRegression")

#' Expectation Maximization Logistic Regression
#'
#' Semi-supervised Logistic regression using expectation maximization, a way of soft self-learning
#' 
#' @usage EMLogisticRegression(X,y,X_u,intercept=TRUE,scale=FALSE)
#'
#' @param X Design matrix of the labeled observations
#' @param y Factor or vector of labels
#' @param X_u Design matrix of the unlabeled observations
#' @param intercept TRUE if an intercept should be added to the model
#' @param scale If TRUE, apply a z-transform to the design matrix X before running the regression
#' @return S4 object of class EMLogisticRegression with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' @export
EMLogisticRegression<-function(X,y,X_u,intercept=TRUE,scale=FALSE) {

  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform

  m<-ncol(X)
  
  # opt_func <- function(w, X, y) {
  #   # Split out the variables
  #   theta<-w[1:3]
  #   w<-w[-(1:3)]
    
  #   # Determine the maximum likelihood solution based on imputed labels
  #   y_i<-c(y,w)
  #   D_i<-data.frame(X[,2:3],classes=y_i)
  #   object<-LogisticRegression(modelform,D_i)
    
  #   # Return the likelihood of the labelled data
  #   return(logLikelihood(object,X[1:length(y),],y))
  # }
  
  # w<-rep(0.0,m+nrow(D_u))
  # opt.result<-optim(w, opt_func,gr=NULL, X,y,method="L-BFGS-B",control=list(fnscale=-1),lower=c(rep(-Inf,m),rep(0.0,nrow(D_u))), upper=c(rep(Inf,m),rep(1.0,nrow(D_u))))
  # browser()
  # theta<-opt.result$par
  
  # print(opt.result)
  # new("EMLogisticRegression",theta=theta,classnames=classnames)
}
