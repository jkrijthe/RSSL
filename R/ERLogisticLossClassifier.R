#' @include LogisticLossClassifier.R
setClass("ERLogisticLossClassifier",
         representation(w="numeric"),
         prototype(name="Entropy Regularized Logistic Loss Classifier"), 
         contains="LogisticLossClassifier")

#' @export
ERLogisticLossClassifier <- function(X,y,X_u=NULL,lambda=0.0,lambda_entropy=1.0,intercept=TRUE, init=NA,scale=FALSE,x_center=FALSE) {

  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  m<-ncol(X)
  
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

  opt_result <- optimx(w, fn=opt_func, gr=NULL, X=X, y=y, X_u=X_u, method="BFGS", control=list(fnscale=1), lower=-Inf, upper=Inf)
  w<-as.numeric(opt_result[1,1:length(w)])
  
  new("ERLogisticLossClassifier",
      modelform=modelform, 
      classnames=classnames, 
      w=w,
      scaling=scaling)
}
