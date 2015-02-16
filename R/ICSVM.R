# ICSVM<-function(X,y,X_u,lambda=0.0,init=NULL) {

#   ## Preprocessing to correct datastructures and scaling  
#   ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
#   X<-ModelVariables$X
#   X_u<-ModelVariables$X_u
#   y<-ModelVariables$y
#   scaling<-ModelVariables$scaling
#   classnames<-ModelVariables$classnames
#   modelform<-ModelVariables$modelform

#   # Convert y to -1,1
#   y<-y*2-3
  
#   X_e<-rbind(X,X_u)
#   opt_func <- function(theta,X,y,X_u) {
#     X_e<-rbind(X,X_u)
#     w<-theta[1:ncol(X)]
#     g<-theta[(ncol(X)+1):(ncol(X+nrow(X_e)))]
#     lagrange<-theta[(ncol(X)+nrow(X_e)+1):length(theta)]
#     #browser()
#     n_l<-nrow(X)
#     n_u<-nrow(X_e)
#     d <- 1 - y * (X %*% w)
#     l<-sum(d[d>0])/n_l+lambda * w %*% w
#     l_u<-0
    
#     l_u <- lagrange %*% ((colSums(((g*X_e)[((X_e %*% w)<1),,drop=FALSE])-colSums(((1-g)*X_e)[((-X_e %*% w)<1),,drop=FALSE]))/n_u + 2*lambda*w))^2
    
#     return(l<-l+l_u)
#   }
  
#   opt_grad <- function(theta, X,y) {

#   }
  
#   if (!is.null(init)) {
#     theta <- c(init,rep(0.5,nrow(X_e)),0.0,rep(0.0,ncol(X)))
#   } else {
#     theta <- c(rep(0.0,ncol(X)),rep(0.5,nrow(X_e)),rep(0.0,ncol(X)))
#   }
#   opt_result <- optim(theta, opt_func, gr=NULL, X, y, X_u, lower=c(rep(-Inf,ncol(X)),rep(0.0,nrow(X_e)),rep(0.0,ncol(X))), upper=c(rep(Inf,ncol(X)),rep(1.0,nrow(X_e)),rep(Inf,ncol(X))),method="L-BFGS-B", control=list(fnscale=1))
#   w<-opt_result$par[1:ncol(X)]
  
#   browser()
#   return(new("SGDSVM", modelform=modelform, classnames=classnames, w=w))
  
# }

# NICSVM<-function(X,y,X_u,lambda=0.0,interceptinit=NULL) {
  
#   ## Preprocessing to correct datastructures and scaling  
#   ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
#   X<-ModelVariables$X
#   X_u<-ModelVariables$X_u
#   y<-ModelVariables$y
#   scaling<-ModelVariables$scaling
#   classnames<-ModelVariables$classnames
#   modelform<-ModelVariables$modelform

#   C<-lambda
  
#   # Convert y to -1,1
#   y<-y*2-3
  
#   # m<-ncol(X)
#   # z<-m+4*n_u
#   # X_e<-rbind(X,X_u)
#   # Dmat<-
#   # dvec<-C*
  
#   # A1<-cbind(matrix(0,n_u,m), diag(3*n_u+n_l))
#   # b1<-matrix(0,4*n_u,1)
#   # A2<-cbind(matrix(0,n_u,m), -diag(n_u), matrix(0,n_u,2*n_u+n_l))
#   # b2<-matrix(-1,n_u,1)
#   # A3<-cbind(y*X, matrix(0,n_l,3*n_u), diag(n_l))
#   # b3<-matrix(1,n_l,1)
#   # A4<-cbind(X, matrix(0,n_u,n_u), diag(n_u), matrix(0,n_u,n_u+n_l))
#   # b4<-matrix(1,n_u,1)
#   # A5<-cbind(X, matrix(0,n_u,2*n_u), diag(n_u), matrix(0,n_u,n_l))
#   # b5<-matrix(1,n_u,1)
  
#   # At<-rbind(A1,A2,A3,A4,A5)
#   # A<-t(A)
#   # bvec<-rbind(b1,b2,b3,b4,b5)
    
#   # solve.QP(Dmat,dvec,A,bvec)
#   # return(new("SGDSVM", modelform=modelform, classnames=classnames, w=w))
  
# }
