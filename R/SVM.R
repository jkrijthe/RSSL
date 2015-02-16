#' @include Classifier.R
setClass("SVM",
         representation(scaling="ANY",alpha="ANY",bias="ANY",kernel="ANY",Xtrain="ANY"),
         prototype(name="Support Vector Machine"),
         contains="Classifier")

#' SVM Classifier
#'
#' @param X Design matrix
#' @param y Labels of the observations
#' @param C Cost variable
#' @param method Estimation procedure c("Dual","Primal","BGD","SGD","Pegasos")
#' @param scale Whether a z-transform should be applied (default: TRUE)
#' @param intercept Whether an intercept should be added (default: TRUE)
#' @return S4 object of type LinearSVM
#' @export
SVM<-function(X, y, C=1, method="Dual",scale=TRUE,intercept=TRUE,kernel=NULL, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=scale,intercept=intercept)
  X<-ModelVariables$X
  y<-ModelVariables$Y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  # Check for two classes and transform to {-1,1}
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  y<-((y-1)*2)-1
  
  eps<-0.1
  ## Start Implementation
  time.begin<-Sys.time()
  if (method=="Dual") {
    if (!require(quadprog)) {stop("quadprog package is required to solve the dual formulation of LinearSVM")}
    
    
    if (intercept) X<-X[,2:ncol(X)]
    
    if (!is.null(kernel)) {
      require(kernlab)
      if (inherits(kernel,"kernel")) {
        Xtrain<-X
        K<-kernelMatrix(kernel,X,X)
      }
    } else {
      K<-X %*% t(X)
    }
    
    Dmat <- (diag(y) %*% K %*% diag(y)) + eps*diag(nrow(X)) #Add small constant to diagonal to ensure numerical PSD
    dvec <- matrix(1, nrow(X), 1)
    Amat <- diag(nrow(X))
    Amat <- t(rbind(y,Amat,-Amat))
    bvec <- c(rep(0,nrow(X)+1),rep(-C,nrow(X)))
    
    opt_result<-solve.QP(Dmat, dvec, Amat, bvec, meq=1)
    alpha<-opt_result$solution
    SVs<-alpha>0.001
    
    bias <- -mean(K[SVs,] %*% alpha - y[SVs])
    #TODO: check this: should we exclude objects not on the margin (alpha=1) in calculating b?
       
  }  else{
    stop("Unknown optimization method.")
  }
  
  time.passed<-Sys.time()-time.begin
  print(time.passed)
  
  return(new("SVM",
             alpha=alpha,
             bias=bias,
             Xtrain=Xtrain,
             kernel=kernel,
             scaling=scaling,
             modelform=modelform,
             classnames=classnames))
}



# LinearSVM here
# 
# else if (method=="SGD") {
#   opt_func <- function(w, X, y) {
#     n_l<-nrow(X)
#     d <- 1 - y * (X %*% w)
#     l<-sum(d[d>0])+lambda * w %*% w
#     #print(l)
#     return(l)
#   }
#   
#   
#   opt_grad <- function(theta, X,y) {
#     
#     theta <- matrix(theta,nrow=ncol(X))
#     
#     # Two-class
#     #t(y-(1-1/(1+exp(X %*% theta)))) %*% X
#     
#     # Multi-class
#     expscore <- cbind(rep(0,nrow(X)), X %*% theta) # Numerators of the probability estimates    
#     
#     for (c in 2:length(classnames)) {
#       theta[,c-1] <- matrix(colSums(X[y==c,,drop=FALSE]), ncol(X),1) - (t(X) %*% (exp(expscore[,c]) / rowSums(exp(expscore))))
#     }
#     as.vector(theta)
#   }
#   
#   
#   opt_result <- optim(w, opt_func, gr=NULL, X, y, method="BFGS", control=list(fnscale=1))
#   w<-opt_result$par
# }

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

#' @rdname boundaryplot-methods
#' @aliases boundaryplot,LinearSVM-method  
setMethod("boundaryplot", signature(object="LinearSVM"), function(object, p) {
  p+geom_abline(intercept = (-object@w[1]/object@w[3]), slope = (-object@w[2]/object@w[3]))
})  
