# Formal class definition
setClass("ExpectationMaximizationLogisticRegression",
         representation(theta="numeric"),
         prototype(name="Expectation Maximization Logistic Regression"), 
         contains="LogisticRegression")

# Constructor 
ExpectationMaximizationLogisticRegression<-function(modelform,D) {
  classname<-all.vars(modelform)[1]
  
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  y<-as.numeric(data.matrix(D_l[,classname]))-1
  X<-data.matrix(cbind(rep(1,nrow(D)),D[,!(colnames(D) %in% c(classname)), drop=FALSE]))
  m<-ncol(X)
  
  opt_func <- function(w, X, y) {
    # Split out the variables
    theta<-w[1:3]
    w<-w[-(1:3)]
    
    # Determine the maximum likelihood solution based on imputed labels
    y_i<-c(y,w)
    D_i<-data.frame(X[,2:3],classes=y_i)
    object<-LogisticRegression(modelform,D_i)
    
    # Return the likelihood of the labelled data
    return(logLikelihood(object,X[1:length(y),],y))
  }
  
  w<-rep(0.0,m+nrow(D_u))
  opt.result<-optim(w, opt_func,gr=NULL, X,y,method="L-BFGS-B",control=list(fnscale=-1),lower=c(rep(-Inf,m),rep(0.0,nrow(D_u))), upper=c(rep(Inf,m),rep(1.0,nrow(D_u))))
  browser()
  theta<-opt.result$par
  
  print(opt.result)
  new("ExpectationMaximizationLogisticRegression",modelform=modelform,D_l=D_l,D_u=D_u,theta=theta)
}

ExpectationMaximizationLogisticRegression(modelform,D_train)