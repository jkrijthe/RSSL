# Formal class definition
setClass("ImplicitlyConstrainedLogisticRegression",
         representation(theta="numeric",constrainedset="ANY"),
         prototype(name="Implicitly Constrained Logistic Regression"), 
         contains="LogisticRegression")

# Helper Method
alllabelings<-function(options,k) {
  if (k==1){ A<-options }
  else {
    A<-c()
    for (i in 1:length(options)) {
      A<-cbind(A, rbind(rep(options[i],length(options)^(k-1)), alllabelings(options,k-1)))
    }
  }
  return(A)
}

# Constructor

# Not raelly fast yet: try to use the previous' models parameter estimate as the initial value in the next round
ImplicitlyConstrainedLogisticRegressionFast<-function(modelform,D) {
  
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  X<-model.matrix(modelform, D)
  y<-(as.factor(D_l[,classname]))
  classnames <- levels(y)
  y<-as.integer(y)
  m<-ncol(X)
  
  y_l<-y[!is.na(y)]
  X_l<-X[!is.na(y),]
  
  # Construct all possible labelings
  labelings<-alllabelings(1:length(classnames),nrow(D_u))
  
  # Determine which labelling gives highest Log-Likelihood on the -supervised- data and return as model
  ll <- -Inf
  h_i <- LogisticRegression(modelform, D_l)
  
  constrainedset<-matrix(nrow=ncol(labelings),ncol=length(h_i@theta))
  init <- h_i@theta
  for (i in 1:ncol(labelings)) {
    D_i <- D
    D_i[,classname] <- c(y_l, labelings[,i])
    h_i <- LogisticRegression(modelform, D_i, init=init)
    init<-h_i@theta
    constrainedset[i,]<-h_i@theta
    ll_i<-logLikelihood(h_i,X_l,y_l)
    
    if (!is.na(ll_i) && ll_i>ll) { 
      ll<-ll_i
      h_trained<-h_i
    }
  }
  
  
  if (ll==0) print(D_l)
  new("ImplicitlyConstrainedLogisticRegression",modelform=modelform,classnames=classnames,D=D,theta=h_trained@theta,constrainedset=constrainedset)
}

setMethod("plot", signature(x="ImplicitlyConstrainedLogisticRegression",y="missing"), function(x) {
  print(x@constrainedset)
  library(ggplot2)
  qplot(x@constrainedset[,1],x@constrainedset[,2])
})