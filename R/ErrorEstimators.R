# Assume we know the marginal distribution
MLupdateCorrectedError<-function(modelform,D_l,P) {
  
  
  classname<-all.vars(modelform)[1]
  
  
}

# Making a discrete assumption to estimate the marginal distribution 
DensityError<-function(modelform,D_l,D_u) {
  library(np)
  Px<-npudens(npudensbw(~x.1+x.2,data=D_l),tdat=D_u[,1:2],edat=D_l[,1:2])
  Pl<-npudens(npudensbw(~x.1+x.2,data=D_l),tdat=D_l[,1:2],edat=D_l[,1:2])
  g<-nearestmean(modelform,D_l)
  errors<-as.integer(predict(g,D_l)==D_l[,classname])
  weights<-(Px$dens/Pl$dens)/sum((Px$dens/Pl$dens))
  #weights<-Px$dens/sum(Px$dens)
  sum(weights*(1-errors))
}

updatedBootstrapError<-function(modelform, D_l, D_u, nboot) {
  library(bootstrap)

  classname<-all.vars(modelform)[1]
  y<-as.numeric(data.matrix(D[,classname]))
  X<-data.matrix(D[,!(colnames(D) %in% c(classname)), drop=FALSE])
  
  library(np)
  Px<-npudens(npudensbw(~x.1+x.2,data=D_l),tdat=D_u[,1:2],edat=D_l[,1:2])
  Pl<-npudens(npudensbw(~x.1+x.2,data=D_l),tdat=D_l[,1:2],edat=D_l[,1:2])
  g<-nearestmean(modelform,D_l)
  errors<-as.integer(predict(g,D_l)==D_l[,classname])
  weights<-(Px$dens/Pl$dens)/sum((Px$dens/Pl$dens))
  
  statistic<-function(D,i){
    if ((nrow(D[-i,]))==0) return(NA) # If all objects are in the bootstrap replicate, we have no test set
    else 1-mean(predict(nearestmean(modelform,D[i,]),D[-i,])==D[-i,classname])
  }
  k<-mean(boot(D_l,statistic,nboot,weights=weights)$t)
  k
  #bootpred(X,y,nboot,nearestmeanxy,predict.nearestmeanxy,function(x1,x2) {1-mean(x1==x2)})
}

updatedBootstrapError2<-function(modelform, D_l, D_u, nboot) {
  library(bootstrap)
  
  classname<-all.vars(modelform)[1]
  y<-as.numeric(data.matrix(D[,classname]))
  X<-data.matrix(D[,!(colnames(D) %in% c(classname)), drop=FALSE])
  
  library(np)
  Px<-npudens(npudensbw(~x.1+x.2,data=D_l),tdat=D_u[,1:2],edat=D_l[,1:2])
  Pl<-npudens(npudensbw(~x.1+x.2,data=D_l),tdat=D_l[,1:2],edat=D_l[,1:2])
  g<-nearestmean(modelform,D_l)
  errors<-as.integer(predict(g,D_l)==D_l[,classname])
  weights<-(Px$dens/sum(Px$dens))
  
  statistic<-function(D,i){
    if ((nrow(D[-i,]))==0) return(NA) # If all objects are in the bootstrap replicate, we have no test set
    else 1-mean(predict(nearestmean(modelform,D[i,]),D[-i,])==D[-i,classname])
  }
  k<-mean(boot(D_l,statistic,nboot,weights=weights)$t)
  k
  #bootpred(X,y,nboot,nearestmeanxy,predict.nearestmeanxy,function(x1,x2) {1-mean(x1==x2)})
}

# Making a parzen density assumption to estimate the marginal distribution
ParzenCorrectedError<-function(modelform,D_l,D_u) {}

# Slightly changed version of crossval
crossvalError<-function(modelform, D, k) {
  library(bootstrap)
  classname<-all.vars(modelform)[1]
  y<-as.numeric(data.matrix(D[,classname]))
  X<-data.matrix(D[,!(colnames(D) %in% c(classname)), drop=FALSE])
  
  crossval(X,y,nearestmeanxy,predict.nearestmeanxy,ngroup=k)
}

bootstrapError<-function(modelform, D, nboot) {
  library(bootstrap)
  
  library(bootstrap)
  classname<-all.vars(modelform)[1]
  y<-as.numeric(data.matrix(D[,classname]))
  X<-data.matrix(D[,!(colnames(D) %in% c(classname)), drop=FALSE])
  
  bootpred(X,y,nboot,nearestmeanxy,predict.nearestmeanxy,function(x1,x2) {1-mean(x1==x2)})
}

nearestmeanxy<-function(x, ...) UseMethod("nearestmeanxy")

nearestmeanxy.default<-function(X,y) {
  classes<-unique(y)
  m<-c()
  for (i in classes) {
    tryCatch(m<-rbind(m,colMeans(X[y==i, ,drop=FALSE])),error=function(e) browser())
  }
  structure(list(m=m,classes=classes),class='nearestmean')
}

predict.nearestmeanxy<-function(object,newdata) {
  if(!inherits(object, "nearestmean")) stop("Not a nearestmean object")
  M<-object$m
  knn(M,newdata,object$classes)
}
