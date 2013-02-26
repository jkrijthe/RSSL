setGeneric("loss",
           function(object, ...) standardGeneric("loss")
)

# Formal class definition
setClass("FisherClassifier",
         representation(theta="matrix",unlabels="ANY",scaling="ANY",optimization="ANY"),
         prototype(name="FisherClassifier",scaling=NULL), 
         contains="Classifier")

# Constructor
FisherClassifier<-function(modelform,D,lambda=0,scale=FALSE) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  X <- model.matrix(modelform, D)
  
  y <- as.factor(data.matrix(D_l[,classname]))
  classnames <- levels(y)
  if (length(classnames)>2){
    y<-(model.matrix(~0+.,data=D[,classname,drop=FALSE]))
  } else {
    y<-as.integer(y)
  }
  
  m<-ncol(X)
  n<-nrow(X)
  
  if (scale) {
    library(pls)
    scaling<-stdize(X[,2:ncol(X),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(X)]<-predict(scaling,X[,2:ncol(X),drop=FALSE])
    
  } else {scaling=NULL}
  
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  theta <- inv(t(X) %*% X + n*lambda*diag(m)) %*% (t(X) %*% y)
  
  new("FisherClassifier",
      modelform=modelform,
      classname=classname,
      classnames=classnames,
      D=D,
      scaling=scaling,
      theta=theta)
}

FisherClassifier.xy<-function(X,y,lambda=0,scale=FALSE) {
  #classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  classnames <- levels(y)
  if (length(classnames)>2){
    y<-(model.matrix(~0+.,data=D[,classname,drop=FALSE]))
  } else {
    y<-as.integer(y)
  }
  
  m<-ncol(X)
  n<-nrow(X)
  
  if (scale) {
    library(pls)
    scaling<-stdize(X[,2:ncol(X),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(X)]<-predict(scaling,X[,2:ncol(X),drop=FALSE])       
  } else {scaling=NULL}
  
  
  if (nrow(X)<ncol(X)) inv <- function(X) { ginv(X) }
  else inv <- function(X) { ginv(X) }
  
  theta <- inv(t(X) %*% X + n*lambda*diag(m)) %*% (t(X) %*% y)
  
  new("FisherClassifier",
      modelform=formula("y~x"),
      classname="",
      classnames=classnames,
      theta=theta,
      scaling=scaling)
}

predictxy<- function(object,X) {
  probs=FALSE
  
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore>1.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  if (probs)
  {
    return(expscore)
  } else return(classes)
}

setMethod("loss", signature(object="FisherClassifier"), function(object, D) {
  X<-model.matrix(object@modelform, D)
  y<-data.matrix(as.integer(D[,object@classname]))
  return(sum((X %*% object@theta - y)^2))
})

setMethod("predict", signature(object="FisherClassifier"), function(object, D, probs=FALSE) {
  
  D[,object@classname] <- 1
  X<-model.matrix(modelform,D)
  
  if (!is.null(object@scaling)) {X[,2:ncol(X)]<-predict(object@scaling,X[,2:ncol(X),drop=FALSE])}
  theta <- matrix(object@theta, nrow=ncol(X))
  expscore <- X %*% theta
  
  # If we need to return classes
  if (length(object@classnames)>2) {
    classes <- factor(apply(expscore,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  } else {
    classes <- factor(as.integer(expscore>1.5)+1,levels=1:length(object@classnames), labels=object@classnames)
  }
  if (probs)
  {
    return(expscore)
  } else return(classes)
})

SemiSupervisedFisherClassifier<-function(modelform,D,lambda=0,lambda1=0,hessian=FALSE,scale=FALSE,method="L-BFGS-B") {
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
  
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { ginv(M) } #Another possibility: chol2inv(chol(M))
  
  Xe <- rbind(X,X_u)
  
  #Added
  if (scale) {
    library(pls)
    scaling<-stdize(Xe[,2:ncol(Xe),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(Xe)]<-predict(scaling,X[,2:ncol(Xe),drop=FALSE])
    X_u[,2:ncol(Xe)]<-predict(scaling,X_u[,2:ncol(Xe),drop=FALSE])
    
    Xe <- rbind(X,X_u)
  } else {scaling=NULL}
  ##End added
  
  C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(m)) 
  F<- X %*% C 
  G <- X_u %*% t(F) %*% F
  
  if (method=="QP") {
    dvec <- X_u %*% C %*% t(X) %*% y
    Dmat <- G %*% t(X_u)
    Amat <- t(rbind(diag(nrow(X_u)), -diag(nrow(X_u))))
    
    # prior constraint
    
    
    bvec <- c(rep(1,nrow(X_u)), rep(-2,nrow(X_u)))
    #browser()
    #LowRankQP(Dmat,dvec,Amat,bvec,uvec,method="PFCF",verbose=FALSE,niter=200)@alpha
    #ipop(dvec, Dmat, l=0, u=0)
    #theta<-solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)$solution
    
    #browser()
    theta<-ipop(-dvec, Dmat, A=rbind(diag(nrow(X_u)),rep(1/nrow(X_u),nrow(X_u))), b=c(rep(1,nrow(X_u)),mean(y)), r=c(rep(1,nrow(X_u)),mean(y)), l=rep(1,nrow(X_u)), u=rep(2,nrow(X_u)))@primal
    unlabels<-theta
    opt_result<-0
    
  }
  else {
  
  
  
  opt_func <- function(theta) {
    theta<-matrix(theta)
    theta <- C %*% t(Xe) %*% rbind(matrix(y),theta)
    if (lambda>0) { return(mean((X %*% theta - y)^2) + lambda * sum(theta^2)) }
    else { return(mean((X %*% theta - y)^2)) }
  }
  
  O1 <- 2/nrow(X) * G %*% t(X) %*% y
  O2 <- 2/nrow(X) * G %*% t(X_u)
  O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
  
  if (lambda>0) {
    O4<- 2 * lambda * X_u %*% C%*% t(F) %*% y
    O5<- 2 * lambda * X_u %*% C %*% C %*% t(X_u)
  }
  
  opt_grad <- function(theta) {
    theta<-matrix(theta)
    if (lambda>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta)
    else return(O1 + O2 %*% theta - O3)
  }

  theta <- rep(1.5,nrow(X_u))
  
  # Bounded optimization
  opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1),hessian=hessian)
  theta<-opt_result$par
  
  unlabels<-theta
  }
  X<-rbind(X,X_u)
  theta <- inv(t(X) %*% X) %*% (t(X) %*% c(y,theta))
  new("FisherClassifier",
      modelform=modelform,
      classname=classname,
      classnames=classnames,
      D=D,
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      optimization=opt_result)
}

SemiSupervisedFisherClassifier.xy<-function(X,y,X_u,modelform,lambda=0,lambda1=0,hessian=FALSE,scale=FALSE,method="L-BFGS-B") {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  classnames <- levels(y)
  if ((nrow(X)+nrow(X_u))<ncol(X)) inv <- function(M) { ginv(M) }
  else inv <- function(M) { ginv(M) } #Another possibility: chol2inv(chol(M))
  y<-as.integer(y)
  m<-ncol(X)
  Xe <- rbind(X,X_u)
  
  #Added
  if (scale) {
    library(pls)
    scaling<-stdize(Xe[,2:ncol(Xe),drop=FALSE], center = TRUE, scale = TRUE)
    X[,2:ncol(Xe)]<-predict(scaling,X[,2:ncol(Xe),drop=FALSE])
    X_u[,2:ncol(Xe)]<-predict(scaling,X_u[,2:ncol(Xe),drop=FALSE])
    
    Xe <- rbind(X,X_u)
  } else {scaling=NULL}
  ##End added
  
  if (method=="QP") {
    solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  }
  
  
  C <- inv(t(Xe) %*% Xe + nrow(Xe)*lambda1*diag(m)) 
  F<- X %*% C 
  G <- X_u %*% t(F) %*% F
  
  opt_func <- function(theta) {
    theta<-matrix(theta)
    theta <- C %*% t(Xe) %*% rbind(matrix(y),theta)
    if (lambda>0) { return(mean((X %*% theta - y)^2) + lambda * sum(theta^2)) }
    else { return(mean((X %*% theta - y)^2)) }
  }
  
  O1 <- 2/nrow(X) * G %*% t(X) %*% y
  O2 <- 2/nrow(X) * G %*% t(X_u)
  O3 <- 2/nrow(X) * X_u %*% t(F) %*% y
  
  if (lambda>0) {
    O4<- 2 * lambda * X_u %*% C%*% t(F) %*% y
    O5<- 2 * lambda * X_u %*% C %*% C %*% t(X_u)
  }
  
  opt_grad <- function(theta) {
    theta<-matrix(theta)
    if (lambda>0) return(O1 + O2 %*% theta - O3 + O4 + O5 %*% theta)
    else return(O1 + O2 %*% theta - O3)
  }
  
  theta <- rep(1.5,nrow(X_u))
  
  # Bounded optimization
  opt_result <- optim(theta, opt_func, gr=opt_grad, method="L-BFGS-B", lower=1.0, upper=2.0, control=list(fnscale=1),hessian=hessian)
  theta<-opt_result$par
  X<-rbind(X,X_u)
  unlabels<-theta
  theta <- inv(t(X) %*% X) %*% (t(X) %*% c(y,theta))
  new("FisherClassifier",
      modelform=modelform,
      classname=classname,
      classnames=classnames,
      D=D,
      theta=theta,
      unlabels=unlabels,
      scaling=scaling,
      optimization=opt_result)
}

setMethod("plot", signature(x="FisherClassifier",y="missing"), function(x) {
  object<-x
  #p<-qplot(object@D[is.na(object@D[,object@classname]),1],object@D[is.na(object@D[,object@classname]),2],color=object@unlabels)
  p<-qplot(object@D[,1],object@D[,2],color=object@D[,object@classname])
  p<-p+geom_abline(intercept = (1.5-x@theta[1])/x@theta[3], slope = -x@theta[2]/x@theta[3])
  return(p)
})

FisherClassifierALL<-function(modelform,D) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  X<-model.matrix(modelform, D)
  y<-as.factor(data.matrix(D_l[,classname]))
  classnames <- levels(y)
  y<-as.integer(y)
  m<-ncol(X)
  
  theta <- inv(t(X) %*% X) %*% (t(X) %*% y)
  
  labelings<-alllabelings(classnames,nrow(D_u))
  #print(labelings)
  # Determine which labelling gives highest Log-Likelihood on the -supervised- data and return as model
  ll<-Inf
  for (i in 1:ncol(labelings)) {
    D_u_i<-D_u
    D_u_i[,classname]<-labelings[,i]
    D_i<-rbind(D_l,D_u_i)
    h_i <- FisherClassifier(modelform, D_i)
    y<-as.integer(data.matrix(D_l[,classname]))
    
    ll_i<-loss(h_i,D_l)
    #print(paste(i," of ",ncol(labelings)))
    
    if (!is.na(ll_i) && ll_i<ll) { 
      ll<-ll_i
      h_trained<-h_i
    }
  }
  
  if (ll==0) print(D_l)
  
  new("FisherClassifier",
      modelform=modelform,
      classname=classname,
      classnames=classnames,
      D=D,
      theta=h_trained@theta)
}
  

#mean(predict(LogisticRegression(modelform,D_l),D_test)==D_test[,classname])
#mean(predict(FisherClassifier(modelform,D_l),D_test)==D_test[,classname])
#mean(predict(SemiSupervisedFisherClassifier(modelform,D_train),D_test)==D_test[,classname])
#mean(predict(FisherClassifierALL(modelform,D_train),D_test)==D_test$classes)

#p<-plot(SemiSupervisedFisherClassifier(modelform,D_train))
#s<-FisherClassifier(modelform,D_l)
#p+geom_abline(intercept = (1.5-s@theta[1])/s@theta[3], slope = -s@theta[2]/s@theta[3])