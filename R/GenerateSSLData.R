#' Generate Sliced Cookie dataset
#'
#' Generate a sliced cookie dataset: a circle with a large margin in the middle.
#'
#' @param n number of observations to generate
#' @param expected TRUE if the large margin equals the class boundary, FALSE if the class boundary is perpendicular to the large margin
#' @param gap Size of the gap
#' @examples
#' data <- generateSlicedCookie(1000,expected=FALSE)
#' plot(data[,1],data[,2],col=data$Class,asp=1)
#' @return A data.frame with n objects from the sliced cookie example
#' @export
generateSlicedCookie<-function(n=100, expected=FALSE, gap=1) {
  X<-mvrnorm(n,c(0,0),diag(c(2,2)))
  
  X[(X[,1]>0),1]<-X[(X[,1]>0),1]+gap
  X[,1]<-X[,1]-0.5
  y<-matrix(-1,n,1)
  if (expected) { y[X[,1]>0,]<-1 }
  else { y[X[,2]>0,]<-1}
  
  return(data.frame(X,Class=factor(y)))
}

#' Generate data from 2 circles
#' 
#' One circle circumscribes the other
#' 
#' @param n integer; Number of examples to generate
#' @param noise_var numeric; size of the variance parameter
#' 
#' @export
generateTwoCircles <- function(n=100, noise_var=0.2) {
  x <- runif(n)*4*pi-2*pi
  X <- rbind(cbind(c(1,2)*cos(x)+noise_var*rnorm(n),c(1,2)*sin(x))+noise_var*rnorm(n))
  y <- factor(rep(c(1,2),length.out = n))
  colnames(X) <- c("X1","X2")
  data.frame(X,Class=y)           
}

#' Generate data from 2 gaussian distributed classes
#' 
#' @param n integer; Number of examples to generate
#' @param d integer; dimensionality of the problem
#' @param var numeric; size of the variance parameter
#' @param expected logical; whether the decision boundary should be the expected or perpendicular
#' 
#' @examples
#' data <- generate2ClassGaussian(1000,expected=FALSE)
#' plot(data[,1],data[,2],col=data$Class,asp=1)
#' @export
generate2ClassGaussian<-function(n=10000,d=100,var=1,expected=TRUE) {
  X<-rbind(mvrnorm(n/2,rep(-1,d),diag(rep(var,d))),mvrnorm(n/2,rep(1,d),diag(rep(var,d))))
  if (expected) {
    y<-rbind(matrix(-1,n/2,1),matrix(1,n/2,1))
  } else {
    y<-factor(as.integer(X[,1]>X[,2])*2-1)
  }
  return(data.frame(X,Class=factor(y)))
}

#' Generate Four Clusters dataset
#'
#' Generate a four clusters dataset
#'
#' @param n number of observations to generate
#' @param distance Distance between clusters (default: 6)
#' @param expected TRUE if the large margin equals the class boundary, FALSE if the class boundary is perpendicular to the large margin
#' @examples
#' data <- generateFourClusters(1000,distance=6,expected=TRUE)
#' plot(data[,1],data[,2],col=data$Class,asp=1)
#' @export
generateFourClusters<-function(n=100,distance=6,expected=FALSE) {
  
  Sigma<-matrix(c(2,1.0,1.8,2),2,2)
  
  X<-rbind(mvrnorm(n,c(-distance,0),Sigma),
           mvrnorm(n,c(0,distance),Sigma),
           mvrnorm(n,c(distance,0),Sigma),
           mvrnorm(n,c(0,-distance),Sigma))
  y<-rep(-1,4*n)
  if (expected) { y[X[,1]>X[,2]]<-1 }
  else { y[X[,1]>-X[,2]]<-1 }
  return(data.frame(X,Class=factor(y)))
}

#' Generate Crescent Moon dataset
#'
#' @param n Number of objects to generate 
#' @param d Dimensionality of the dataset
#' @param sigma Noise added 
#' @examples
#' data<-generateCrescentMoon(150,2,1)
#' plot(data$X1,data$X2,col=data$Class,asp=1)
#' @export
generateCrescentMoon<-function(n=100,d=2,sigma=1) {
  
  if (d!=2) { stop("Crescent Moon dataset is currently only available in 2D")}
  x<-runif(n,0,pi)
  c1<-cbind(5*cos(x)-2.5+rnorm(n)*sigma,10*sin(x)-2.5+rnorm(n)*sigma)
  x<-runif(n,pi,2*pi)
  c2<-cbind(5*cos(x)+2.5+rnorm(n)*sigma,10*sin(x)+2.5+rnorm(n)*sigma)
  y<-factor(c(rep("+",n),rep("-",n)))
  
  data.frame(Class=y, rbind(X1=c1,X2=c2))
}

#' Plot 2D classification problem with missing labels
#'
#' @param X feature values for observations
#' @param y labels for observations
#' @export 
clplot<-function(X,y) {
  if (is.factor(y)) {
    if (any(is.na(y))){
      levels(y)<-c(levels(y),"NA")
      y[is.na(y)]<-"NA"
    }
    colScale <- scale_colour_manual(values = c("orange","purple","darkgrey"))
  }
  if (is.factor(y)) p<-qplot(X[,1],X[,2],color=y,asp=1,size=y)+colScale+scale_size_manual(values=c(5,5,2))+scale_shape()   # Shape depends on cond
  else p<-qplot(X[,1],X[,2],color=y,asp=1,size=5)                                                                                                  
  p<-p + theme(
    panel.background = element_rect(fill = "transparent",colour = NA), 
    panel.grid.minor = element_line(colour="lightgrey",size=0.5,linetype=2), 
    panel.grid.major = element_line(colour="lightgrey",size=0.5,linetype=2)
  )
  p<-p+xlab(colnames(X)[1])
  p<-p+ylab(colnames(X)[2])
  return (p)
}
