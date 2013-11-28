#' @include Classifier.R
NULL

#' LinearSVM Class  
setClass("LinearSVM",
         representation(w="ANY",scaling="ANY"),
         prototype(name="Support Vector Machine"),
         contains="Classifier")

#' Linear SVM Classifier
#'
#' @param X Design matrix
#' @param y Labels of the observations
#' @param C Cost variable
#' @param method Estimation procedure c("Dual","Primal","BGD","SGD","Pegasos")
#' @param scale Whether a z-transform should be applied (default: TRUE)
#' @param intercept Whether an intercept should be added (default: TRUE)
#' @return S4 object of type LinearSVM
#' @export
LinearSVM<-function(X, y, C=1, method="Dual",scale=TRUE,intercept=TRUE) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform

  y<-((y-1)*2)-1
  
  eps<-0.0000000001
  ## Start Implementation
  time.begin<-Sys.time()
  if (method=="Dual") {
    
    if (intercept) X<-X[,2:ncol(X)]
    Dmat<- (diag(y) %*% X %*% t(diag(y) %*% X)) + eps*diag(nrow(X)) #Add small constant to diagonal to ensure numerical PSD
    dvec<-matrix(1, nrow(X), 1)
    Amat<-diag(nrow(X))
    Amat<-t(rbind(y,Amat,-Amat))
    bvec<-c(rep(0,nrow(X)+1),rep(-C,nrow(X)))
    opt_result<-solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    alpha<-opt_result$solution
    SVs<-alpha>0.001
    w<-matrix(alpha*y,1,nrow(X)) %*% X
    
    b<--mean(X[SVs,] %*% t(w) - y[SVs])
    w<-c(b, w)
    
    
  } else if (method=="Primal") {
    
    Dmat<-bdiag(matrix(0,1,1),diag(ncol(X)-1),matrix(0,nrow(X),nrow(X))) + eps*diag(ncol(X)+nrow(X))
    dvec<-c(rep(0,ncol(X)), rep(C,nrow(X)))
    Amat<-cbind(matrix(0,nrow(X),ncol(X)),diag(nrow(X))) #Slack variable bigger than 0
    Amat<-rbind(Amat,cbind(diag(y) %*% X, diag(nrow(X))))
    Amat<-t(Amat)
    bvec<-c(rep(0,nrow(X)), rep(1,nrow(X)))            
    opt_result<-solve.QP(Dmat, -dvec, Amat, bvec)
    w<-opt_result$solution[1:ncol(X)]
    
  } else if (method=="BGD") {
    
    w <- rep(0.0,ncol(X)) #Initial parameter values
    
    opt_func <- function(w, X, y) {
      n_l<-nrow(X)
      d <- 1 - y * (X %*% w)
      l<-C*sum(d[d>0]) +  w %*% w
      return(l)
    }
    
    
    opt_result <- optim(w, opt_func, gr=NULL, X, y, method="BFGS", control=list(fnscale=1))
    w<-opt_result$par
    
  } else if (method=="SGD") {
    
    w <- rep(0.0,ncol(X)) #Initial parameter values
    
    sgd_grad<-function(w,x,y) {
      if (1 - y * (x %*% w)>0){
        return(C*-y*x + 2*w)
      } else {
        return(rep(0,length(w)))
      }
    }
    
    converged<-FALSE
    i<-1
    while (!converged) {
      iv<-((i-1)%%nrow(X))+1 # Number in sample
      alpha<-1/i
      
      w<-w-alpha*sgd_grad(w,X[iv,],y[iv])
      
      if (i==100000) {
        converged<-TRUE
      }
      
      i<-i+1
    }
  } else if (method=="Pegasos") {
    
    #TODO: fix bias term
    
    w <- rep(0.0,ncol(X)) #Initial parameter values
    
    lambda<-1/(nrow(X)*C) # Convert C to lambda
    k<-nrow(X)
    
    for (i in 1:1000) {
      
      d <- 1 - y * (X %*% w)
      
      alpha<-1/(lambda*i)
      if (!any(d>0)){break}
      w <- (1-lambda*alpha)*w + alpha/k * colSums(diag(y[d>0]) %*% X[d>0,])
      w<-matrix(w)
      w <- min(1, (1/sqrt(lambda))/norm(w,"F"))*w# Project it back
      w<-matrix(w)
    }
    print(i)
    
 
  } else {
    stop("Unknown optimization method.")
  }
  
  time.passed<-Sys.time()-time.begin
  print(time.passed)
  
  return(new("LinearSVM",
               w=w,
              scaling=scaling,
             modelform=modelform,
             classnames=classnames))
}

#' predict method for LinearSVM
#'
#' Predict class of new observations using a LinearSVM
#' @rdname predict-methods
#' @aliases predict,LinearSVM-method                                                                                             
setMethod("predict", signature(object="LinearSVM"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=TRUE)
  X<-ModelVariables$X
  
  w <- matrix(object@w,nrow=ncol(X))
  
  result<-factor(as.numeric(X %*% w>0),levels=0:1)
  levels(result)<-object@classnames
  return(result)
})

#' Loss method for LinearSVM
#'
#' Hinge loss on new objects of a trained LinearSVM
#'
#' @usage loss(object, X, probs=FALSE)
#' @usage loss(object, newdata, lambda=0, probs=FALSE)
#'
#' @param object Object of class LinearSVM
#' @param X Design matrix of the test data, intercept term is added within the function
#' @param y Vector with true classes of the test data
#' @param newdata data.frame object with test data
#' @return numeric of the total loss on the test data
#' @rdname loss-methods
#' @aliases loss,LinearSVM-method                                                                                            
setMethod("loss", signature(object="LinearSVM"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,object@scaling,intercept=TRUE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  
  w <- matrix(object@w,nrow=ncol(X))
  
  y<-y*2-3
  d <- 1 - y * (X %*% w)
  l<-sum(d[d>0])
  return(l)
})

#' boundaryplot method for LinearSVM
#'
#' Predict class of new observations using a LinearSVM
#' @rdname boundaryplot-methods
#' @aliases boundaryplot,LinearSVM-method  
setMethod("boundaryplot", signature(object="LinearSVM"), function(object, p) {
  p+geom_abline(intercept = (-object@w[1]/object@w[3]), slope = (-object@w[2]/object@w[3]))
})  

# D<-GenerateSlicedCookie(100,expected=FALSE)
# p<-clplot(as.matrix(D[,1:2]),factor(D$y))
# 
# g1<-LinearSVM(formula(y~.),D,1000,method="Primal")
# print(g1)
# print(boundaryplot(g1,p))
# print(loss(g1,D))
# 
# g2<-LinearSVM(formula(y~.),D,1000,method="Dual")
# print(g2)
# print(boundaryplot(g2,p))
# print(loss(g2,D))
# 
# g3<-LinearSVM(formula(y~.),D,1000,method="BGD")
# print(g3)
# print(boundaryplot(g3,p))
# print(loss(g3,D))
# 
# g4<-LinearSVM(formula(y~.),D,1000,method="SGD")
# print(g4)
# print(boundaryplot(g4,p))
# print(loss(g4,D))
# 
# g5<-LinearSVM(formula(y~.),D,1000,method="Pegasos")
# print(g5)
# print(boundaryplot(g5,p))
# print(loss(g5,D))
# 
# time.begin<-Sys.time()
# LibLinear(as.matrix(D[,1:2]),D[,3])
# print(Sys.time()-time.begin)
# 
# time.begin<-Sys.time()
# g<-svm(as.matrix(D[,1:2]),D[,3],kernel="linear",cost=1000)
# print(Sys.time()-time.begin)
