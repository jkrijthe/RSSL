# Formal class definition
setClass("EntropyRegularizedLogisticRegression",
         representation(theta="numeric"),
         prototype(name="Entropy Regularized Logistic Regression"), 
         contains="LogisticRegression")

# Constructor
EntropyRegularizedLogisticRegression <- function(modelform,D,lambda2=1.0,lambda=0.0,init=NA) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  D_u[,classname] <- 1
  X<-model.matrix(modelform, D_l)
  X_u <- model.matrix(modelform, D_u)
  y<-as.factor(data.matrix(D_l[,classname]))
  classnames <- levels(y)
  y<-as.integer(y)
  m<-ncol(X)
  
  
  opt_func <- function(theta) {
    theta <- matrix(theta,nrow=ncol(X))
    
    # Likelihood term
    expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates
    ll <- sum(-log(rowSums(exp(expscore)))) # Denominators of the probability estimates, summed
    
    for (c in 1:length(classnames)) {
      ll <- ll + sum(expscore[y==c,c]) # Sum the numerators for each class
    }
    
    # Entropy term
    # Sum over all unlabelled points and classes, take proabbility times log probability
    expscore <- cbind(rep(0,nrow(X_u)), X_u %*% theta)
    sum_exp<-rowSums(exp(expscore))
    
    
    
    ent<-0
    for (c in 1:length(classnames)) {
      ent <- ent + sum( (exp(expscore[,c])/sum_exp) * (expscore[,c]-log(sum_exp))) # Sum the numerators for each class
    }
    #print(ll+ent)
    if (is.nan(ll+ent)) return(0.0)
    if (is.infinite(ll+ent)) return(0.0)
    return(ll + lambda2*ent + lambda * matrix(theta,nrow=1) %*% t(matrix(theta,nrow=1)))
  }
  
  opt_grad <- function(theta, X,y) {
    # not correct for this function
    theta <- matrix(theta,nrow=ncol(X))
    
    # Two-class
    #t(y-(1-1/(1+exp(X %*% theta)))) %*% X
    
    # Multi-class
    expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates    
    
    for (c in 2:length(classnames)) {
      theta[,c-1] <- matrix(colSums(X[y==c,,drop=FALSE]),ncol(X),1) - (t(X) %*% (exp(expscore[,c]) / rowSums(exp(expscore))))
    }
    print()
    print(theta)
    as.vector(theta)
  }
  
  if (is.na(init[1])) {
    theta <- rep(0.0,ncol(X)*(length(classnames)-1))
  } else {
    theta<-init
  }
  
  opt_result <- optim(theta, opt_func, gr=NULL, method="BFGS", control=list(fnscale=-1))
  theta<-opt_result$par
  
  new("EntropyRegularizedLogisticRegression",
      modelform=modelform,
      classname=classname,
      classnames=classnames,
      D=D,
      theta=theta)
}