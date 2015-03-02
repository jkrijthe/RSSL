split_dataset_ssl<-function(X, y, frac_train=0.8, frac_ssl=0.8) {
  n<-length(y)
  idx_train<-sample(1:n,size=ceiling(frac_train*n))
  n_lab<-length(idx_train) #Number of labeled objects
  idx_labeled<-idx_train[sample(1:n_lab,size=ceiling((1-frac_ssl)*n_lab))]
  
  return(list(X=X[idx_labeled,,drop=FALSE],
              y=y[idx_labeled], 
              X_u=X[setdiff(idx_train,idx_labeled),,drop=FALSE],
              y_u=y[setdiff(idx_train,idx_labeled)],
              X_test=X[-idx_train,,drop=FALSE],
              y_test=y[-idx_train])
  )
}

data <- GenerateSlicedCookie(300,expected=TRUE,gap=1)
X <- model.matrix(y~.-1,data)
y <- factor(data$y)
X<-X[,2:1]

problem <- split_dataset_ssl(X,y,frac_ssl=0.95)
# Example dataset:
# X<-matrix(c(0,-2,0,2),2,2,byrow=TRUE)
# y<-c(-1,1)
# X_u<-matrix(c(1,0.5,-1,1),2,2,byrow=TRUE)
# c(y,rep(1,nrow(X_u)))

X <- problem$X
y <- problem$y
ynum <- model.matrix(~y-1,data.frame(y=problem$y))[,1]*2-1
yenum <- model.matrix(~y-1,data.frame(y=unlist(list(problem$y,problem$y_u))))[,1]*2-1
X_u <- problem$X_u
Xe<-rbind(X,X_u)

g_sup <- solve_svm(X %*% t(X),ynum,C=1)
g_trans<-TSVM_CCCP(X=X,y=y,X_u=X_u,C=1,Cstar=100,scale=FALSE)
g_trans2<-TSVM_CCCPlin(X=X,y=y,X_u=X_u,C=1,Cstar=1,scale=FALSE)
g_supext <- solve_svm(Xe %*% t(Xe),yenum,C=1)


w1 <- g_sup$alpha %*% (ynum*X)
w2 <- g_trans$alpha %*% rbind(X,X_u,X_u,colMeans(X_u))
w3 <- g_supext$alpha %*% (yenum*Xe)
w4 <- g_trans2$w

plot(X[,1],X[,2],col=factor(y),asp=1)
points(X_u[,1],X_u[,2],col="darkgrey",pch=16,cex=0.5)
abline(-g_sup$b/w1[2],-w1[1]/w1[2],lty=2)
abline(((1-g_sup$b)/w1[2]),-w1[1]/w1[2],lty=2) # +1 Margin
abline(((-1-g_sup$b)/w1[2]),-w1[1]/w1[2],lty=2) # -1 Margin


abline(-g_trans$b/w2[2],-w2[1]/w2[2],lty=1)
abline(-g_supext$b/w3[2],-w3[1]/w3[2],lty=3)
abline(-g_trans$b/w4[2],-w4[1]/w4[2],lty=1,lwd=3,col="blue")

# w1 <- LeastSquaresClassifier(X, y, intercept = TRUE,x_center = FALSE, scale=FALSE)@theta
# w2 <- ICLeastSquaresClassifier(X, y, X_u, intercept = TRUE, x_center = FALSE, scale=FALSE)@theta
# plot(X[,1],X[,2],col=factor(y),asp=1)
# points(X_u[,1],X_u[,2],col="darkgrey",pch=16,cex=0.5)
# abline((1.5-w1[1])/w1[3],-w1[2]/w1[3],lty=2)
# abline((1.5-w2[1])/w2[3],-w2[2]/w2[3],lty=1)
