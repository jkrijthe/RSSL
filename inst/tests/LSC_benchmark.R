library(microbenchmark)

n<-100000
a<-0.5
b<-1
X<-matrix(rnorm(n = 2*n),ncol=2)
y<-factor(X%*%c(a,b)+rnorm(n=n)<0)

LeastSquaresClassifier(X,y,method="inverse")@theta
LeastSquaresClassifier(X,y,method="QR")@theta
LeastSquaresClassifier(X,y,method="Normal")@theta
LeastSquaresClassifier(X,y,method="BFGS")@theta
LeastSquaresClassifier(X,y,method="BFGSCPP")@theta
LeastSquaresClassifier(X,y,method="CG")@theta
LeastSquaresClassifier(X,y,method="Newton")@theta
LeastSquaresClassifier(X,y,method="CPP")@theta

microbenchmark(
  #LeastSquaresClassifier(X,y,method="inverse"),
  #LeastSquaresClassifier(X,y,method="QR"),
  #LeastSquaresClassifier(X,y,method="QR2"),
  LeastSquaresClassifier(X,y,method="Normal"),
  LeastSquaresClassifier(X,y,method="BFGS"),
  LeastSquaresClassifier(X,y,method="BFGSCPP"),
  LeastSquaresClassifier(X,y,method="CPP"),
  times=10)
