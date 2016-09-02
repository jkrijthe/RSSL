#' Generate Sliced Cookie dataset
#'
#' Generate a sliced cookie dataset: a circle with a large margin in the middle.
#'
#' @family RSSL datasets
#' 
#' @param n integer; number of observations to generate
#' @param expected logical; TRUE if the large margin equals the class boundary, FALSE if the class boundary is perpendicular to the large margin
#' @param gap numeric; Size of the gap
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
#' @family RSSL datasets
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

#' Generate data from 2 alternating classes
#' 
#' Two clusters belonging to three classes: the cluster in the middle belongs to one class and the two on the outside to the others.
#' 
#' @family RSSL datasets
#' 
#' @param n integer; Number of examples to generate
#' @param d integer; dimensionality of the problem
#' @param var numeric; size of the variance parameter
#' 
#' @examples
#' data <- generateABA(n=1000,d=2,var=1)
#' plot(data[,1],data[,2],col=data$Class,asp=1)
#' @export
generateABA<-function(n=100,d=2,var=1) {
  if (n%%4!=0) {stop("Please set n as a multiple of 4.")}
  
  X<-rbind(mvrnorm(n/4,rep(-1,d),var*diag(d)),
           mvrnorm(n/4,rep(+1,d),var*diag(d)),
           mvrnorm(n/2,rep(0,d),var*diag(d)))
  y<-rbind(matrix(-1,n/2,1),matrix(1,n/2,1))
  
  return(data.frame(X,Class=factor(y)))
}

#' Generate data from 2 Gaussian distributed classes
#' 
#' @family RSSL datasets
#' 
#' @param n integer; Number of examples to generate
#' @param d integer; dimensionality of the problem
#' @param var numeric; size of the variance parameter
#' @param expected logical; whether the decision boundary should be the expected or perpendicular
#' 
#' @examples
#' data <- generate2ClassGaussian(n=1000,d=2,expected=FALSE)
#' plot(data[,1],data[,2],col=data$Class,asp=1)
#' @export
generate2ClassGaussian<-function(n=10000,d=100,var=1,expected=TRUE) {
  X<-rbind(mvrnorm(n/2,rep(-1,d),var*diag(d)),
           mvrnorm(n/2,rep(1,d),var*diag(d)))
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
#' @family RSSL datasets
#' 
#' @param n integer; Number of observations to generate
#' @param distance numeric; Distance between clusters (default: 6)
#' @param expected logical; TRUE if the large margin equals the class boundary, FALSE if the class boundary is perpendicular to the large margin
#' 
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
#' Generate a "crescent moon"/"banana" dataset
#' 
#' @family RSSL datasets
#' 
#' @param n integer; Number of objects to generate 
#' @param d integer; Dimensionality of the dataset
#' @param sigma numeric; Noise added 
#' 
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

#' Generate Intersecting Spirals
#' 
#' @family RSSL datasets
#' 
#' @param n integer; Number of objects to generate per class
#' @param sigma numeric; Noise added 
#' @examples
#' data <- generateSpirals(100,sigma=0.1)
#' #plot3D::scatter3D(data$x,data$y,data$z,col="black")
#' @export
generateSpirals <- function(n=100,sigma=0.1) {
  z <- runif(n)*5
  x <- sin(z)
  y <- cos(z)
  
  z2 <- runif(n)*5
  x2 <- -sin(z2)
  y2 <- -cos(z2)
  data.frame(x=c(x,x2)+sigma*rnorm(2*n),y=c(y,y2)+sigma*rnorm(2*n),z=c(z,z2)+sigma*rnorm(2*n),Class=rep(factor(c("A","B")),each=n))
}

#' Generate Parallel planes
#' 
#' @family RSSL datasets
#' 
#' @param n integer; Number of objects to generate 
#' @param classes integer; Number of classes
#' @param sigma double; Noise added
#'  
#' @examples
#' library(ggplot2)
#' df <- generateParallelPlanes(100,3)
#' ggplot(df, aes(x=x,y=y,color=Class,shape=Class)) +
#'  geom_point()
#'
#' @export
generateParallelPlanes <- function(n=100,classes=3,sigma=0.1) {
  x <- c()
  y <- c()
  for (i in 1:classes) {
    yn <- rep(i,n)
    xn <- runif(n)
    yn <- yn+sigma*rnorm(n)
    x <- c(x,xn)
    y <- c(y,yn)
  }
  
  data.frame(x,y,Class=rep(factor(c(LETTERS[1:classes])),each=n))
}
