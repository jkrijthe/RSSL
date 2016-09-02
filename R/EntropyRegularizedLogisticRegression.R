#' @include LogisticRegression.R
setClass("EntropyRegularizedLogisticRegression",
         representation(w="numeric"),
         prototype(name="Entropy Regularized Logistic Regression"), 
         contains="LogisticRegression")

#' Entropy Regularized Logistic Regression
#'
#' R Implementation of entropy regularized logistic regression implementation as proposed by Grandvalet & Bengio (2005). An extra term is added to the objective function of logistic regression that penalizes the entropy of the posterior measured on the unlabeled examples.
#'
#' @param lambda l2 Regularization
#' @param lambda_entropy Weight of the labeled observations compared to the unlabeled observations
#' @param init Initial parameters for the gradient descent
#' @inheritParams BaseClassifier
#' @references Grandvalet, Y. & Bengio, Y., 2005. Semi-supervised learning by entropy minimization. In L. K. Saul, Y. Weiss, & L. Bottou, eds. Advances in Neural Information Processing Systems 17. Cambridge, MA: MIT Press, pp. 529-536.
#' @return S4 object of class EntropyRegularizedLogisticRegression with the following slots:
#' \item{w}{weight vector}
#' \item{classnames}{the names of the classes}
#' @example inst/examples/example-EntropyRegularizedLogisticRegression.R
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
  
  if (is.na(init[1])) {
    w <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else if (init=="supervised") {
    w <- optim(w, 
               fn=loss_logisticregression,
               gr=grad_logisticregression, 
               X=X, y=y, lambda=lambda,
               classnames=classnames,
               method="BFGS", 
               control=list(fnscale=-1))$par
  } else {
    w <- init
  }
  
  # Optimization
  opt_result <- optim(w, 
                      fn=loss_erlr, 
                      gr=grad_erlr, 
                      X, y, X_u, 
                      lambda=lambda, 
                      lambda_entropy=lambda_entropy,
                      classnames=classnames,
                      method="BFGS",
                      control = list(fnscale=-1))
  
  w<-opt_result$par
  
  new("EntropyRegularizedLogisticRegression",
      modelform=modelform, 
      classnames=classnames, 
      w=w,
      intercept=intercept,
      opt_result=opt_result,
      scaling=NULL)
}

loss_erlr <- function(w, X, y, X_u, classnames, lambda, lambda_entropy) {
  loss_logisticregression(w,X,y,
                          classnames = classnames, 
                          lambda=lambda) + 
    lambda_entropy * loss_entropy(w,X_u)
}

grad_erlr <- function(w, X, y, X_u, classnames, lambda, lambda_entropy) {
  grad_logisticregression(w, X, y,
                          classnames = classnames, 
                          lambda=lambda) + 
    lambda_entropy * grad_entropy(w,X_u)
}

loss_entropy <- function(w, X_u) {
  w <- matrix(w,ncol(X_u))
  inner <- cbind(rep(0,nrow(X_u)), X_u %*% w)
  G <- exp(inner - as.numeric(logsumexp(inner)))
  sum(G * log(G))
}  

grad_entropy <- function(w, X_u) {
  w <- matrix(w,ncol(X_u))
  inner <- cbind(rep(0,nrow(X_u)), X_u %*% w)
  G <- exp(inner - as.numeric(logsumexp(inner)))

  G[G>1-1e-16] <- 1-1e-16
  
  weight <- G*(1-G) * (log(G)-log(1-G))
  weight <- weight[,2]
  
  if(any(is.nan(weight))) stop("Numerical issues in gradient calculation.")
  
  return(t(X_u) %*% weight)
}
