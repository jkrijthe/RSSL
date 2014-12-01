#' Implicitly Constained Logistic Loss Classifier
#'
#' Implicitly constrained semisupervised learning with logistic loss. This is equivalent to finding a weighting of the unlabeled objects belonging to either of the two classes.
#'
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix of the unlabeled data, intercept term is added within the function
#' @param lambda1 Regularization parameter in the unlabeled+labeled data regularized least squares
#' @param lambda2 Regularization parameter in the labeled data only regularized least squares
#' @param intercept TRUE if an intercept should be added to the model
#' @param scale If TRUE, apply a z-transform to all observations in X and X_u before running the regression
#' @param init Starting parameter vector for gradient descent
#' @param verbose logical; Verbose output
#' @param lambda3 numeric; Prior controlling the deviation of mean predictions form the overall mean
#' @param trueprob numeric; Set overal mean
#' @param ... additional arguments
#' @return S4 object of class ICLogisticLossClassifier with the following slots:
#' \item{theta}{weight vector}
#' \item{classnames}{the names of the classes}
#' \item{modelform}{formula object of the model used in regression}
#' \item{scaling}{a scaling object containing the paramters of the z-transforms applied to the data}
#' \item{optimization}{the object returned by the optim function}
#' \item{unlabels}{the labels assigned to the unlabeled objects}
#' @export
ICLogisticLossClassifier <- function(X, y, X_u, lambda1=0, lambda2=0, intercept=TRUE, scale=FALSE, init=NA, verbose=TRUE, lambda3=0,trueprob=NULL, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
  X<-ModelVariables$X
  y<-ModelVariables$y
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { solve(M) } #Another possibility: chol2inv(chol(M))
  
  if (!is.null(trueprob)) {
    mean_y<-trueprob
  } else {
    mean_y<-mean(y)-1
  }
  
  y <- (y-1.5)*2
  
  
  L_sl <- function(w,X,y,lambda) {
    loss <- sum(log(matrix(1,nrow(X),1)+exp(- y * (X %*% w)))) + lambda2 * w[-1] %*% w[-1]
    # print(loss)
    return(loss)
  }
  
  grad_sl <- function(w, X, y,lambda) {
    w <- matrix(w,nrow=ncol(X))
    # Numerators of the probability estimates    
    weightings <- -y * (exp(- y * (X %*% w))/(matrix(1,nrow(X),1)+exp(- y * (X %*% w))))
    as.vector(t(X) %*% weightings + 2* c(0,lambda2 *w[-1]))
  }
  
  L_ssl <- function(w,g, X, y, X_u) {
    g<-as.vector(g)
    # g<-rep(0,nrow(X_u))
    loss <- L_sl(w,X,y) + sum(g * log(matrix(1,nrow(X_u),1)+exp(-1 * (X_u %*% w))) + (1-g) * log(matrix(1,nrow(X_u),1)+exp((X_u %*% w))))
    return(as.numeric(loss))
  }
  
  grad_ssl <- function(w,g, X, y, X_u) {
    
    g<-as.vector(g)
    # g<-rep(0,nrow(X_u))
    grad_sl(w,X,y) + t(X_u) %*% (g*(-1 * (exp(-1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(-1 * (X_u %*% w))))) + (1-g) * (1 * (exp(1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(1 * (X_u %*% w)))))) 
  }
  
  loss<-function(g,X,y,X_u) {
    w<-psi(g,X,y,X_u)
    # if (verbose) cat("Loss: ", L_sl(w,X,y),"\n")
    as.numeric(L_sl(w,X,y) + lambda3*(mean(g)-mean_y)^2)
  }
  
  psi<-function(g,X,y,X_u) {
    g<-g
    w <- rep(0.0,ncol(X)*(length(classnames)-1))
    w<-optim(w, L_ssl, gr=grad_ssl, g, X, y, X_u, method="BFGS", control=list(fnscale=1))$par
  }
  
  gradient <- function(g,X,y,X_u) {
    w<-psi(g,X,y,X_u)
    # if (verbose) cat("Gradient norm: ", c(rcond(-Dw(w,g,X,y,X_u)),norm(Dg(w,X,y,X_u),"F")),"\n")
    
    ans<-t(grad_sl(w,X,y)) %*% (ginv(-Dw(w,g,X,y,X_u)) %*% Dg(w,X,y,X_u)) + lambda3*2*(rep(mean(g),length(g))-rep(mean_y,length(g)))
    # true<-grad(function(u) {loss(u,X,y,X_u)},g)
    # print(sum(abs(ans-true)))
    # print(ans)
    # print(true)
    # browser()
    # print(cbind(t(ans),t(t(true))))
    # grad(function(u) {grad_ssl(w,u,X,y,X_u)[2]},g)
    # Dg(w,X,y,X_u)[2,]
    # grad(function(u) {L_sl(u,X,y)},w)
    # grad_sl(w,X,y)
    # grad(function(u) {grad_ssl(u,g,X,y,X_u)[3]},w)
    # Dw(w,g,X,y,X_u)[3,]

    return(ans)
  }
    
  Dw <- function(w,g,X,y,X_u) {
    g<-as.vector(g)
    
    weightings_sl <- ((y)^2)*(exp(-y * (X %*% w))/(matrix(1,nrow(X),1)+exp(-y * (X %*% w)))^2)
    weightings_ssl <- g * (exp(-1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(-1 * (X_u %*% w)))^2) + (1-g) * (exp(1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(1 * (X_u %*% w)))^2)
    # weightings_ssl <- (1-2*g)*(exp(1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(1 * (X_u %*% w)))^2) # Same as above
    
    if (lambda2>0) t(X) %*% diag(as.vector((weightings_sl))) %*% X + t(X_u) %*% diag(as.vector((weightings_ssl))) %*% X_u + 2*diag(c(0,rep(1,length(w)-1)))
    else t(X) %*% diag(as.vector((weightings_sl))) %*% X + t(X_u) %*% diag(as.vector((weightings_ssl))) %*% X_u
    # t(X) %*% X + t(X_u) %*% X_u
  }
  
  Dg <- function(w,X,y,X_u) {
    w<-matrix(w,length(w),1)
    # weightings<- (exp(-1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(-1 * (X_u %*% w)))) + (exp(1 * (X_u %*% w))/(matrix(1,nrow(X_u),1)+exp(1 * (X_u %*% w))))
    # browser()
    # -t(diag(as.vector(weightings)) %*% X_u)
    -t(X_u)
  }
  
  
  if (is.na(init[1])) {
    g <- rep(0.0,nrow(X_u))
  } else {
    g<-init
  }
#     iterations<-1000
#     alpha<-0.1
#     for (i in 1:iterations) {
#       g <- g - alpha*gradient(g,X,y,X_u)
#       print(loss(g,X,y,X_u))
#     }
#   
  opt_result <- optim(g, fn=loss, gr=gradient, X=X, y=y,X_u=X_u, method="L-BFGS-B", lower=0.0, upper=1.0, control=list(fnscale=1))
  # g<-as.numeric(opt_result[1,1:length(g)])
  g<-opt_result$par
  # print(g)
  w<-psi(g,X,y,X_u)
  #print(L_sl(w,X,y))
  
  return(new("LogisticLossClassifier", classnames=classnames, w=w,modelform=modelform,scaling=scaling))
}
