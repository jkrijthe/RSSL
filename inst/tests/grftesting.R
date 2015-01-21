library(magrittr)
library(ggplot2)
generateTwoCircles <- function(n=100, noise_var=0.2) {
  x <- runif(n)*4*pi-2*pi
  X <- rbind(cbind(c(1,2)*cos(x)+noise_var*rnorm(n),c(1,2)*sin(x))+noise_var*rnorm(n))
  y <- factor(rep(c(1,2),length.out = n))
  data.frame(X=X,y=y)           
}

generateTwoCircles(200,0.05) %>%
  ggplot(aes(X.1,X.2,color=y)) %>%
  + geom_point() %>%
  + coord_equal()

W <- exp(-as.matrix(dist(Xin))^2/0.1)
u<-harmonic_function(W,yin)
data.frame(X=Xin,y=c(yin,u)) %>%
ggplot(aes(X.X.1,X.X.2,color=y)) %>%
+ geom_point() %>%
+ coord_equal()



harmonic_function <- function(W,Y) {
  l <- nrow(Y) # the number of labeled points
  n <- nrow(W) # total number of points
  
  # the graph Laplacian L=D-W
  L <- diag(colSums(W)) - W;
  
  # the harmonic function.
  fu <- -solve(L[(l+1):n, (l+1):n]) %*% L[(l+1):n, 1:l] %*% Y
  
  # compute the CMN solution
  #q = colSums(Y)+1 % the unnormalized class proportion estimate from labeled data, with Laplace smoothing
  #TODO: fu_CMN = fu .* repmat(q./sum(fu), n-l, 1);
}


data <- generateTwoCircles(200, 0.05) 
Xin <- as.matrix(data[,1:2])
yin <- model.matrix(~y-1,data)[1:2,1,drop=FALSE]
W <- exp(-as.matrix(dist(Xin))^2/0.1)

u<-harmonic_function(W,yin)

data.frame(Xin, posterior=c(yin,u)) %>%
  ggplot(aes(X.1,X.2,colour=posterior)) %>%
  + geom_point(size=5,alpha=0.8) %>%
  + coord_equal()


all(harmonic_function(W,yin)==harmonic_function(W,yin))
library(microbenchmark)
microbenchmark(harmonic_function(W,yin),harmonic_function_cpp(W,yin))
