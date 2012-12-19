# Formal class definition
setClass("ImplicitlyConstrainedLogisticRegression",representation(theta="matrix"),prototype(name="Implicitly Constrained Logistic Regression"), contains="LogisticRegression")

# Constructor 
ImplicitlyConstrainedLogisticRegression<-function(modelform,D) {
  classname<-all.vars(modelform)[1]
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  
  y<-as.numeric(data.matrix(D_l[,classname]))-1
  X<-data.matrix(cbind(rep(1,nrow(D)),D[,!(colnames(D) %in% c(classname)), drop=FALSE]))
  
  #w<-rep(0.0,nrow(D_u))
  #opt.result<-optim(w,logLik.ImplicitlyConstrainedLogisticRegression,gr=NULL,X,y,method="L-BFGS-B",control=list(fnscale=-1),lower=0, upper=1)
  
  theta<-rep(0.0,nrow(D_u)+3)
  opt.result<-optim(w,logLik2.ImplicitlyConstrainedLogisticRegression,gr=NULL,X,y,method="L-BFGS-B",control=list(fnscale=-1),lower=c(rep(-Inf,3),rep(0.0,nrow(D_u))), upper=c(rep(Inf,3),rep(1.0,nrow(D_u))))
  
  
  print(opt.result)
  new("ImplicitlyConstrainedLogisticRegression",modelform=modelform,D_l=D_l,D_u=D_u,theta=theta)
}


logLik.ImplicitlyConstrainedLogisticRegression<-function(w,X,y) { 
  # Determine the maximum likelihood solution based on imputed labels
  y_i<-c(y,w)
  D_i<-data.frame(X[,2:3],classes=y_i)
  theta<-LogisticRegression(modelform,D_i)
  
  # Return the likelihood of the labelled data
  logLik.LogisticRegression(theta,X[1:length(y),],y)
}

logLik2.ImplicitlyConstrainedLogisticRegression<-function(theta,X,y) {
  w<-theta[-(1:3)]
  theta<-theta[1:3]
  y<-c(y,w)
  
  sum( y*(X %*% theta) - log(1+exp(X%*%theta)) )
}