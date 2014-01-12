#' Generate Sliced Cookie dataset
#'
#' Generate a sliced cookie dataset: a circle with a large margin in the middle.
#'
#' @usage GenerateSlicedCookie(n=100,expected=FALSE)
#'
#' @param n number of observations to generate
#' @param expected TRUE if the large margin equals the class boundary, FALSE if the class boundary is perpendicular to the large margin
#' @return A data.frame with n objects from the sliced cookie example
#' @export
GenerateSlicedCookie<-function(n=100,expected=FALSE) {
  X<-mvrnorm(n,c(0,0),diag(c(2,2)))
  
  X[(X[,1]>0),1]<-X[(X[,1]>0),1]+1
  X[,1]<-X[,1]-0.5
  y<-matrix(-1,n,1)
  if (expected) { y[X[,1]>0,]<-1 }
  else { y[X[,2]>0,]<-1}
  
  return(data.frame(X,y))
}

#' @export
Generate2ClassGaussian<-function(n=10000,d=100,var=1,expected=TRUE) {
  X<-rbind(mvrnorm(n/2,rep(-1,d),diag(rep(var,d))),mvrnorm(n/2,rep(1,d),diag(rep(var,d))))
  if (expected) {
    y<-rbind(matrix(-1,n/2,1),matrix(1,n/2,1))
  } else {
    y<-factor(as.integer(X[,1]>X[,2])*2-1)
  }
  return(data.frame(X,y))
}

# D<-GenerateSlicedCookie(1000,expected=FALSE)
# clplot(as.matrix(D[,1:2]),factor(D$y))
#

#' Generate Four Clusters dataset
#'
#' Generate a four clusters dataset
#'
#' @usage GenerateFourClusters(n=100,distance=6,expected=FALSE)
#'
#' @param n number of observations to generate
#' @param distance Distance between clusters (default: 6)
#' @param expected TRUE if the large margin equals the class boundary, FALSE if the class boundary is perpendicular to the large margin
#'
#' @export
GenerateFourClusters<-function(n=100,distance=6,expected=FALSE) {
  
  Sigma<-matrix(c(2,1.0,1.8,2),2,2)
  
  X<-rbind(mvrnorm(n,c(-distance,0),Sigma),
           mvrnorm(n,c(0,distance),Sigma),
           mvrnorm(n,c(distance,0),Sigma),
           mvrnorm(n,c(0,-distance),Sigma))
  y<-rep(-1,4*n)
  if (expected) { y[X[,1]>X[,2]]<-1 }
  else { y[X[,1]>-X[,2]]<-1 }
  return(data.frame(X,y))
}
# D<-GenerateFourClusters(1000,distance=6,expected=TRUE)
# clplot(as.matrix(D[,1:2]),factor(D$y))
# 
# GenerateTwoGaussians<-function(n,distance,expected=FALSE) {
#   Sigma<-matrix(c(2,0,0,2),2,2)
#   X<-rbind(mvrnorm(n,c(-distance,0),Sigma),
#            mvrnorm(n,c(distance,0),Sigma))
#   y<-rep(-1,2*n)
#   if (expected) { y[X[,1]<0]<-1 }
#   else { y[X[,2]<0]<-1 }
#   return(data.frame(X,y))
# }
# D<-GenerateTwoGaussians(1000,distance=6,expected=FALSE)
# clplot(as.matrix(D[,1:2]),factor(D$y))

#' Plot 2D classification problem with missing labels
#'
#' @param X feature values for observations
#' @param y labels for observations
#' @export 
clplot<-function(X,y) {
  if (is.factor(y)) {
#     levels(y)<-c(levels(y),"NA")
#     y[is.na(y)]<-"NA"
    colScale <- scale_colour_manual(values = c("orange","purple","black"))
  }
  if (is.factor(y)) p<-qplot(X[,1],X[,2],color=y,asp=1,size=y,shape=y)+colScale+scale_size_manual(values=c(3,3,0.5))+scale_shape()   # Shape depends on cond
  else p<-qplot(X[,1],X[,2],color=y,asp=1)                                                                                                  
  p<-p + theme(
    panel.background = element_rect(fill = "transparent",colour = NA), 
    panel.grid.minor = element_line(colour="lightgrey",size=0.5,linetype=2), 
    panel.grid.major = element_line(colour="lightgrey",size=0.5,linetype=2)
  )
  p<-p+xlab(colnames(X)[1])
  p<-p+ylab(colnames(X)[2])
  return (p)
}
# 
# SampleDatasetSSL<-function(model,D,p){
#   classname<-all.vars(model)[1]
#   n<-nrow(D)
#   idx<-sample(1:n,floor(n*p))
#   D[idx,classname]<-rep(NA,floor(n*p))
#   return(D)
# }
# 
# D<-GenerateSlicedCookie(1000)
# 
# Dssl<-SampleDatasetSSL(formula(y~.),D,0.99)
# 
# g<-SGDSVM(formula(y~.),Dssl)
# g_sl<-LogisticRegression(formula(y~.),Dssl)
# g_ssl<-EntropyRegularizedLogisticRegression(formula(y~.),Dssl,lambda2=10)
# 
# g_sl<-LeastSquaresClassifier(formula(y~.),Dssl)
# g_ssl<-ICLeastSquaresClassifier(formula(y~.),Dssl)
# 
# p<-clplot(as.matrix(Dssl[,1:2]),factor(Dssl$y))
# p
# p<-boundaryplot(g_sl,p)
# p
# p<-boundaryplot(g_ssl,p)
# p
# p<-boundaryplot(g,p)
# p
