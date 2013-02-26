# Formal class definition
setClass("ImplicitlyConstrainedNearestMeanClassifier",
         representation(parameters.allowed="matrix"),
         prototype(name="Implicitly Constrained Nearest Mean Classifier"),
         contains="NearestMeanClassifier")

# Constructor Method
ImplicitlyConstrainedNearestMeanClassifier <- function(modelform, D) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  D_u[,classname] <- 1
  X<-model.matrix(modelform, D_l)
  X_u <- model.matrix(modelform, D_u)
  y<-as.factor(data.matrix(D_l[,classname]))
  classnames <- levels(y)
  y<-as.integer(y)
  X<-X[,2:ncol(X)]
  X_u<-X_u[,2:ncol(X_u)]
  
  m<-ncol(X)
  
  y<-matrix(y,nrow=length(y),ncol=1)
  
  opt_func<-function(y_u) {
#     y_u<-matrix(y_u, nrow=length(y_u), ncol=1)
#     y_u<-y_u-1
     y<-y-1
#     mu1<- (t(X) %*% y + t(X_u) %*% y_u)/(sum(y)+sum(y_u))
#     mu0<-(t(X) %*% (1-y) + t(X_u) %*% (1-y_u))/(sum((1-y))+sum((1-y_u)))
#     
#     p1=(sum(y)+sum(y_u))/(length(y)+length(y_u))
#     p0=1-p1
     #sigma2 <- (t((t(X)-mu1 %*% rep(1,length(y))) %*% y) %*% ((t(X)-mu1 %*% rep(1,length(y))) %*% y) + t((t(X)-mu0 %*% rep(1,length(y))) %*% (1-y)) %*% ((t(X)-mu0 %*% rep(1,length(y))) %*% (1-y)))/((length(y)+length(y_u))*m)
     
    mu1<-matrix(y_u[1:2],nrow=2,ncol=1)
    mu0<-matrix(y_u[3:4],nrow=2,ncol=1)
    p1<-y_u[5]
    p0<-1-y_u[5]
    sigma2<-y_u[6]
      
    
    #browser()
    ll<-(length(y))*m*log(sqrt(2*pi*sigma2)) + sum((1-y))*log(p0)+ sum((y))*log(p1) -0.5/sigma2 * t((t(X)-mu1 %*% rep(1,length(y))) %*% y) %*% ((t(X)-mu1 %*% rep(1,length(y))) %*% y) -0.5/sigma2* t((t(X)-mu0 %*% rep(1,length(y))) %*% (1-y)) %*% ((t(X)-mu0 %*% rep(1,length(y))) %*% (1-y))
    
    print(ll)
    return(ll)
  }
  
  theta <- rep(0.1,6)
  opt_result <- optim(theta, opt_func, gr=NULL, method="L-BFGS-B", control=list(fnscale=1),hessian=hessian)
  
  
  new("ImplicitlyConstrainedNearestMeanClassifier",means=h_trained@means,classname=classname,classnames=nm_solution@classnames,featurenames=featurenames,parameters.allowed=parameters.allowed,modelform=modelform)
}

#Constructor Slow
ImplicitlyConstrainedNearestMeanClassifierALL <- function(modelform, D) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  nm_solution<-NearestMeanClassifier(modelform,D_l)
  featurenames<-nm_solution@featurenames

  labelings<-alllabelings(levels(D[,classname]),nrow(D_u))

  parameters.allowed<-matrix(nrow=ncol(labelings),ncol=length(featurenames)*2+1)
  # Determine which labelling gives highest Log-Likelihood on the -supervised- data and return as model
  ll<--Inf
  for (i in 1:ncol(labelings)) {
    D_u_i<-D_u
    D_u_i[,classname]<-labelings[,i]
    D_i<-rbind(D_l,D_u_i)
    h_i <- NearestMeanClassifier(modelform,D_i)
   
    ll_i<-logLik(h_i,D_l)
    parameters.allowed[i,]<-c(as.vector(h_i@means),ll_i)
    
    if (!is.na(ll_i) && ll_i>ll) { 
      ll<-ll_i
      h_trained<-h_i
    }
  }
  new("ImplicitlyConstrainedNearestMeanClassifier",means=h_trained@means,classname=classname,classnames=nm_solution@classnames,featurenames=featurenames,parameters.allowed=parameters.allowed,modelform=modelform)
}

#setMethod("ImplicitlyConstrainedNearestMeanClassifier", signature(modelform="formula"), ImplicitlyConstrainedNearestMeanClassifier)

# Plot function
setMethod("plot", signature(x="ImplicitlyConstrainedNearestMeanClassifier"), function(x) {
  object<-x
  pl<-qplot(object@parameters.allowed[,1],object@parameters.allowed[,2],color=object@parameters.allowed[,3])
  pl+geom_point(x=object@means[1,], y=object@means[2,], shape=1)
})