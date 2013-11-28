#' @include Classifier.R
setClass("SGDSVM", 
         representation(w="numeric"), 
         prototype(name="SGDSVM"), 
         contains="Classifier")

SGDSVM<-function(X,y, init=NA, lambda=0.0) {
  # Make sure we get a matrix from the model representation
  if (is.formula(X)) {
    modelform<-X
    list2env(SSLDataFrameToMatrices(X,y,intercept=TRUE),environment())
    classnames<-classnames
  } else {
    modelform<-NULL
    classnames<-1:length(unique(y))
  }
  
  w <- rep(0.0,ncol(X))
  
  # Convert y to -1,1
  y<-y*2-3
  
  
  
  #hingeloss<-function() max(0,1-w %*% X[i,])
  
  opt_func <- function(w, X, y) {
    n_l<-nrow(X)
    d <- 1 - y * (X %*% w)
    l<-sum(d[d>0])+lambda * w %*% w
    #print(l)
    return(l)
  }

  
  opt_grad <- function(theta, X,y) {
    
    theta <- matrix(theta,nrow=ncol(X))
    
    # Two-class
    #t(y-(1-1/(1+exp(X %*% theta)))) %*% X
    
    # Multi-class
    expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates    
    
    for (c in 2:length(classnames)) {
      theta[,c-1] <- matrix(colSums(X[y==c,,drop=FALSE]), ncol(X),1) - (t(X) %*% (exp(expscore[,c]) / rowSums(exp(expscore))))
    }
    as.vector(theta)
  }
  
  
  opt_result <- optim(w, opt_func, gr=NULL, X, y, method="BFGS", control=list(fnscale=1))
  w<-opt_result$par
  return(new("SGDSVM", modelform=modelform, classnames=classnames, w=w))
  
}

#' Loss method for SGDSVM
#'
#' Hinge loss on new objects of a trained SGDSVM
#'
#' @usage loss(object, X, probs=FALSE)
#' @usage loss(object, newdata, lambda=0, probs=FALSE)
#'
#' @param object Object of class LeastSquaresClassifier
#' @param X Design matrix of the test data, intercept term is added within the function
#' @param y Vector with true classes of the test data
#' @param newdata data.frame object with test data
#' @return numeric of the total loss on the test data
#' @rdname loss-methods
#' @aliases loss,SGDSVM-method                                                                                  
setMethod("loss", signature(object="SGDSVM"), function(object, newdata, y=NULL) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    if (is.null(y)) { stop("No labels supplied.")}
    X<-newdata
  }
  
  
  w <- matrix(object@w,nrow=ncol(X))
  
  y<-y*2-3
  d <- 1 - y * (X %*% w)
  l<-sum(d[d>0])
  return(l)
})                                                                              


#' Predict method for SGDSVM
#' 
#' Predict the class of new objects for a normal based classifier
#'
#' @usage predict(object, newdata)
#'
#' @param object object of class SGDSVM
#' @param newdata a matrix of data.frame, depending on what was used to train the classifier, with new objects to be classified
#' @return a vector with predicted classes
#' @rdname predict-methods
#' @aliases predict,SGDSVM-method
setMethod("predict", signature(object="SGDSVM"), function(object, newdata,probs=FALSE) {
  if (!is.null(object@modelform)) {
    list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
  } else {
    if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
    X<-newdata
  }
  
  w <- matrix(object@w, nrow=ncol(X))
  return(factor(as.integer(((X %*% w)>0))+1,levels=1:2,labels=object@classnames))
  # If we need to return classes
  classes <- factor(apply(probabilities,1,which.max),levels=1:length(object@classnames), labels=object@classnames)
  if (probs)
  {
    return(probabilities)
  } else return(classes)
})

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

#' Boundary plot method for SGDSVM
#'
#' @param object SGDSVM object
#' @param p ggplot object of classification problem generated by clplot
#' @rdname boundaryplot-methods
#' @aliases boundaryplot,SGDSVM-method
setMethod("boundaryplot", signature(object="SGDSVM"), function(object, p) {
  p+geom_abline(intercept = (-object@w[1]/object@w[3]), slope = (-object@w[2]/object@w[3]))
})

# g<-0.5
# 
# x=-0.4
# plot(seq(-10,10,0.01),sapply(seq(-10,10,0.01),function(w){max(g*(1+w*x),0)+max((1-g)*(1-w*x),0)}),'l')
# 
# w=4
# plot(seq(0,1,0.01),sapply(seq(0,1,0.01),function(g){max(g*(1+w*x),0)+max((1-g)*(1-w*x),0)}),'l')
