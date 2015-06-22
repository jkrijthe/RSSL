#' @include LogisticRegression.R
setClass("EntropyRegularizedLogisticRegression",
         representation(w="numeric"),
         prototype(name="Entropy Regularized Logistic Regression"), 
         contains="LogisticRegression")

#' Entropy Regularized Logistic Regression
#'
#' Entropy regularized logistic regression as introduced by \cite{Grandvalet2005}.
#'
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix of the unlabeled data, intercept term is added within the function
#' @param lambda l2 Regularization
#' @param lambda_entropy Weight of the labeled observations compared to the unlabeled observations
#' @param intercept Whether an intercept should be added to the model
#' @param init Initial parameters for the gradient descent
#' @param scale logical; whether the data should be normalized
#' @param x_center logical; whether the data should be centered
#' 
#' @return S4 object of class EntropyRegularizedLogisticRegression with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' @export
EntropyRegularizedLogisticRegression <- function(X,y,X_u=NULL,lambda=0.0,lambda_entropy=1.0,intercept=TRUE, init=NA,scale=FALSE,x_center=FALSE) {

  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  m<-ncol(X)
  
  opt_func <- function(theta,X,y,X_u) {
#     object<-new("LogisticRegression", theta=theta)
#     return(loss(object,X,y) + lambda * object@theta %*% object@theta)
    theta <- matrix(theta,nrow=ncol(X))
    
    # Likelihood term
    expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Denominator of the probability estimates
    ll <- sum(-log(rowSums(exp(expscore)))) # Numerator of the probability estimates, summed
    
    for (c in 1:length(classnames)) {
      ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
    }
    
    # Entropy term
    # Sum over all unlabelled points and classes, take proabbility times log probability
    expscore <- cbind(rep(0,nrow(X_u)), X_u %*% theta)
    sum_exp<-rowSums(exp(expscore))
    
    ent<-0
    for (c in 1:length(classnames)) {
      #TODO: is the +1 correct?
      ent <- ent + sum( (1+(exp(expscore[,c])/sum_exp)) * (expscore[,c]-log(sum_exp))) # Sum the numerators for each class
    }
    #print(ll+ent)
    if (is.nan(ll+ent)) return(0.0)
    if (is.infinite(ll+ent)) return(0.0)
    
    return(ll/nrow(X) + lambda_entropy*ent/nrow(X_u) + lambda * matrix(theta,nrow=1) %*% t(matrix(theta,nrow=1)))
  }
  
  opt_grad <- function(theta, X,y) {
    
    theta <- matrix(theta,nrow=ncol(X))
    
    # Two-class
    #t(y-(1-1/(1+exp(X %*% theta)))) %*% X
    
    # Multi-class
    expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates    
    
    for (c in 2:length(classnames)) {
      theta[,c-1] <- matrix(colSums(X[y==c,,drop=FALSE]), ncol(X),1) - (t(X) %*% (exp(expscore[,c]) / rowSums(exp(expscore))))
    }
    as.vector(theta)
  }
  
  if (is.na(init[1])) {
    theta <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else {
    theta<-init
  }
  
  opt_result <- optim(theta, opt_func, gr=NULL, X, y, X_u, method="BFGS", control=list(fnscale=-1))
  #print(opt_result$par)
  theta<-opt_result$par
  
  new("EntropyRegularizedLogisticRegression",
      modelform=modelform, 
      classnames=classnames, 
      w=theta,
      intercept=intercept,
      scaling=NULL)
}

erlr_loss <- function(theta,X,y,X_u,lambda,lambda_entropy) {
  #     object<-new("LogisticRegression", theta=theta)
  #     return(loss(object,X,y) + lambda * object@theta %*% object@theta)
  theta <- matrix(theta,nrow=ncol(X))
  
  # Likelihood term
  min_inner <- - X %*% theta
  expscore <- cbind(rep(1,nrow(X)), min_inner) # Denominator of the probability estimates
  ll <- sum(min_inner*y) -sum(log(rowSums(exp(expscore)))) # Numerator of the probability estimates, summed
  
  # Entropy term
  # Sum over all unlabelled points and classes, take proabbility times log probability
  min_inner <- - X_u %*% theta
  expscore <- cbind(rep(1,nrow(X_u)), min_inner)
  sum_exp <- rowSums(expscore)
  ent <- -sum(log(sum_exp)/sum_exp) + sum(expscore[,2]*(min_inner-log(sum_exp))/sum_exp)
  
  
  ent<-0
  for (c in 1:ncol(expscore)) {
    #TODO: is the +1 correct?
    ent <- ent + sum( (1+(exp(expscore[,c])/sum_exp)) * (expscore[,c]-log(sum_exp))) # Sum the numerators for each class
  }
  #print(ll+ent)
  if (is.nan(ll+ent)) return(0.0)
  if (is.infinite(ll+ent)) return(0.0)
  
  return(ll/nrow(X) + lambda_entropy*ent/nrow(X_u) + lambda * t(theta[,2]) %*% theta[,2])
}

#' @include LogisticLossClassifier.R
setClass("ERLogisticLossClassifier",
         representation(w="numeric"),
         prototype(name="Entropy Regularized Logistic Loss Classifier"), 
         contains="LogisticLossClassifier")

#' Entropy Regularized Logistic Loss Classifier
#' 
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix of the unlabeled data, intercept term is added within the function
#' @param lambda numeric; Regularization parameter for the supervised loss
#' @param lambda_entropy numeric; Parameter for the entropy term
#' @param intercept TRUE if an intercept should be added to the model
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param init Starting parameter vector for gradient descent
#' @param x_center logical; whether the data should be centered
#' @param ... additional arguments
#' 
#' @export
ERLogisticLossClassifier <- function(X,y,X_u=NULL,lambda=0.0,lambda_entropy=1.0,intercept=TRUE, init=NA,scale=FALSE,x_center=FALSE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  Y<-ModelVariables$Y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  m<-ncol(X)
  
  y <- (Y-1.5)*2
  
  opt_func <- function(w,X,y,X_u) {
    w <- matrix(w,nrow=ncol(X))
    loss <- sum(log(matrix(1,nrow(X),1)+exp(-y * (X %*% w)))) + lambda * w[-1] %*% w[-1]
    
    # Entropy term
    # Sum over all unlabelled points and classes, take proabbility times log probability
    expscore <- cbind(-X_u %*% w, X_u %*% w)
    sum_exp<- rowSums(exp(expscore))
    
    ent<-0
    for (c in 1:length(classnames)) {
      ent <- ent + sum( (1+(exp(expscore[,c])/sum_exp)) * (expscore[,c]-log(sum_exp))) # Sum the numerators for each class
    }
    
    if (is.nan(loss+ent)) return(0.0)
    if (is.infinite(loss+ent)) return(0.0)
    
    return(as.numeric(loss/nrow(X) + lambda_entropy*ent/nrow(X_u)))
  }
  
  opt_grad <- function(w, X,y) {
    w <- matrix(w,nrow=ncol(X))
    # Numerators of the probability estimates    
    weightings <- -y * (exp(- y * (X %*% w))/(matrix(1,nrow(X),1)+exp(- y * (X %*% w))))
    as.vector(t(X) %*% weightings + lambda *c(0,w[-1]))
    
    #TODO: derivative of the entropy
  }
  
  if (is.na(init[1])) {
    w <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else {
    w<-init
  }
  
  
  opt_result <- optim(w, fn=opt_func, gr=NULL, X=X, y=y, X_u=X_u, method="BFGS", control=list(fnscale=1), lower=-Inf, upper=Inf)
  w<-as.numeric(opt_result$par[1:length(w)]) #TODO: changed this: is it still correct?
  
  new("ERLogisticLossClassifier",
      modelform=modelform, 
      classnames=classnames, 
      w=w,
      scaling=scaling)
}

