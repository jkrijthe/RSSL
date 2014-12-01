# Test CCCP
library(e1071)
ind<-sample(1:20,10)
X <- as.matrix(data[ind,1:2])
y <- as.integer(as.character(data$y[ind]))

# X<-scale(X,scale=apply(X,2, function(x) { (abs(max(x)-min(x))) }))

C<-1
g_bench <- svm(X, y, scale=FALSE,kernel="linear",cost=C,type="C-classification")
g_sol <- solve_svm(X%*% t(X), y, C=C)

SVs<-(g_sol$alpha>0.001 & g_sol$alpha<(C-0.0001))
plot(X[,1],X[,2],col=factor(y),cex=1+(g_sol$alpha>0.000001 & g_sol$alpha<(C-0.000001)))
w<-g_sol$alpha%*% (y*X)
abline((-g_sol$b/w[2]),-w[1]/w[2]) # Decision boundary
abline(((-g_sol$b+w[1]/norm(w))/w[2]),-w[1]/w[2]) # +1 Margin
abline(((-g_sol$b-w[1]/norm(w))/w[2]),-w[1]/w[2]) # -1 Margin

print(g_bench$index)
print(as.numeric(g_sol$alpha*y)[g_bench$index])
print(as.numeric(g_bench$coefs)) # How to set the coefficients
print(-g_bench$rho)
print(g_sol$b)


