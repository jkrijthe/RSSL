#' @include Classifier.R
setClass("GRFClassifier",
         representation(theta="matrix",
                        responsibilities="ANY",
                        scaling="ANY",
                        optimization="ANY",
                        intercept="ANY",
                        Xtrain="matrix",
                        ytrain="ANY",
                        eta="numeric",
                        sigma="numeric",
                        predictions="factor"),
         prototype(name="GRFClassifier",scaling=NULL), 
         contains="Classifier")

#' Gaussian Random Fields and Harmonic functions
#' 
#' @param adjacency_kernel kernlab::kernel; kernel object
#' @param sigma parameter for kernel if no kernel is given
#' @param eta The influence of the external classifier (not currently used)
#' @param y_u Labels given by external classifier (Not used)
#' @param class_mass_normalization Should the Class Mass Normalization heuristic be applied? (default: TRUE)
#' @references Zhu, X., Ghahramani, Z. & Lafferty, J., 2003. Semi-supervised learning using gaussian fields and harmonic functions. In Proceedings of the 20th International Conference on Machine Learning. pp. 912-919.
#' @inheritParams BaseClassifier
#' @example tests/examples/exampleGRFClassifier.R
#' @export
GRFClassifier<-function(X,y,X_u,adjacency_kernel=NULL,sigma=0.1,eta=0.1,class_mass_normalization=TRUE,scale=FALSE,x_center=FALSE,y_u=NULL) {

  mv <- PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- mv$X
  X_u <- mv$X_u
  scaling <- mv$scaling
  classnames <- mv$classnames
  modelform <- mv$modelform
  y <- mv$y
  Y <- factor_to_dummy(y)
  
  Xin <- rbind(X,X_u)
  
  if (!is.null(adjacency_kernel)) {
    W <- kernelMatrix(adjacency_kernel,Xin,Xin)
  } else {
    W <- exp(-as.matrix(dist(Xin))^2/sigma) # A possible Kernel
  }
  
  harmonic_f <- harmonic_function(W,Y)
  responsibilities <- harmonic_f$fu
  
  if (class_mass_normalization) {
    class_ind <- which_rowMax(responsibilities)
  } else {
    class_ind <- which_rowMax(harmonic_f$fu_cmn)
  }
  
  predictions <- factor(class_ind,levels=1:ncol(Y),labels=classnames)
  
  return(new("GRFClassifier",
             modelform=modelform,
             scaling=mv$scaling,
             responsibilities=responsibilities,
             predictions=predictions,
             Xtrain=X,
             classnames=classnames,
             ytrain=y,
             sigma=sigma,
             eta=eta
             ))
}

#' @rdname rssl-predict
#' @aliases predict,GRFClassifier-method
setMethod("predict", signature(object="GRFClassifier"),function(object,newdata,...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
  X<-ModelVariables$X
  
  # Train the classifier here
  t_class <- GRFClassifier(object@Xtrain,object@ytrain,X,object@sigma,object@eta)
  class_ind <- as.integer(t_class@unlabels < 0.5)
  factor(class_ind,levels=0:1,labels=object@classnames)
})

#' Direct R Translation of Xiaojin Zhu's Matlab code to determine harmonic solution
#' @param W matrix; weight matrix where the fist L rows/column correspond to the labeled examples.
#' @param Y matrix; l by c 0,1 matrix encoding class assignments for the labeled objects
#' @return The harmonic solution, i.e. eq (5) in the ICML paper, with or without class mass normalization
harmonic_function <- function(W,Y) {
  l <- nrow(Y) # the number of labeled points
  n <- nrow(W) # total number of points
  
  # the graph Laplacian L=D-W
  L <- diag(colSums(W)) - W;
  
  # the harmonic function.
  fu <- -solve(L[(l+1):n, (l+1):n],L[(l+1):n, 1:l]  %*% Y)
  
  # Class Mass Normalization
  q <- colSums(Y)+1 # the unnormalized class proportion estimate from labeled data, with Laplace smoothing
  fu_cmn <- fu * matrix(rep((q/colSums(fu)),n-l),nrow=n-l,byrow = TRUE)
  
  
  return(list(fu=fu,fu_cmn=fu_cmn))
}
