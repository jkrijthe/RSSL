#' @include Classifier.R
setClass("svmlinClassifier",
         representation(weights="numeric",algorithm="numeric",intercept="logical"),
         prototype(name="svmlinClassifier"), 
         contains="Classifier")

#' svmlin implementation by Sindhwani & Keerthi (2006)
#'
#' R interface to the svmlin code by Vikas Sindhwani and S. Sathiya Keerthi for fast linear transductive SVMs.
#'
#' The codes to select the algorithm are the following: 0. Regularized Least Squares Classification 1. SVM (L2-SVM-MFN) 2. Multi-switch Transductive SVM (using L2-SVM-MFN) 3. Deterministic Annealing Semi-supervised SVM (using L2-SVM-MFN).
#'
#' @param X Matrix or sparseMatrix containing the labeled feature vectors, without intercept
#' @param y factor containing class assignments
#' @param X_u Matrix or sparseMatrix containing the unlabeled feature vectors, without intercept
#' @param algorithm integer; Algorithm choice, see details (default:1)
#' @param lambda double; Regularization parameter lambda (default 1)
#' @param lambda_u double; Regularization parameter lambda_u (default 1)
#' @param max_switch integer; Maximum number of switches in TSVM (default 10000)
#' @param pos_frac double; Positive class fraction of unlabeled data  (default 0.5)
#' @param Cp double; Relative cost for positive examples (only available with algorithm 1)
#' @param Cn double; Relative cost for positive examples (only available with algorithm 1)
#' @inheritParams BaseClassifier
#'
#' @references Vikas Sindhwani and S. Sathiya Keerthi. Large Scale Semi-supervised Linear SVMs. Proceedings of ACM SIGIR, 2006
#'  @references V. Sindhwani and S. Sathiya Keerthi. Newton Methods for Fast Solution of Semi-supervised Linear SVMs. Book Chapter in Large Scale Kernel Machines, MIT Press, 2006
#' @examples
#' data(svmlin_example)
#' t_svmlin_1 <- svmlin(svmlin_example$X_train[1:50,],
#'                  svmlin_example$y_train,X_u=NULL, lambda = 0.001)
#' t_svmlin_2 <- svmlin(svmlin_example$X_train[1:50,],
#'                        svmlin_example$y_train,
#'                        X_u=svmlin_example$X_train[-c(1:50),], 
#'                        lambda = 10,lambda_u=100,algorithm = 2)
#'                        
#' # Calculate Accuracy
#' mean(predict(t_svmlin_1,svmlin_example$X_test)==svmlin_example$y_test)
#' mean(predict(t_svmlin_2,svmlin_example$X_test)==svmlin_example$y_test)
#' 
#' data(testdata)
#' 
#' g_svm <- SVM(testdata$X,testdata$y)
#' g_sup <- svmlin(testdata$X,testdata$y,testdata$X_u,algorithm = 3)
#' g_semi <- svmlin(testdata$X,testdata$y,testdata$X_u,algorithm = 2)
#' 
#' mean(predict(g_svm,testdata$X_test)==testdata$y_test)
#' mean(predict(g_sup,testdata$X_test)==testdata$y_test)
#' mean(predict(g_semi,testdata$X_test)==testdata$y_test)
#' @family RSSL classifiers
#' @export
svmlin <- function(X, y, X_u=NULL, algorithm=1, lambda=1, lambda_u=1, max_switch=10000, pos_frac=0.5, Cp=1.0, Cn=1.0,verbose=FALSE,intercept=TRUE,scale=FALSE,x_center=FALSE) {
  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  y <- ModelVariables$y
  
  X <- Matrix(X, sparse = TRUE)
  if (!is.null(X_u)) {
    X_u <- Matrix(X_u, sparse = TRUE)
  }
  
  stopifnot(nrow(X)==length(y))
  stopifnot(is.factor(y))
  stopifnot(length(levels(y))==2)

  y <- as.numeric(y)*2-3
  
  # Combine feature matrices, add intercept and transpose them to conform to the C++ datastructure
  if (is.null(X_u) || algorithm<2) {
    if (intercept) {
      X <- cbind2(X,1)
      X_u <- cbind2(X_u,1)
    }
    Xall <- Matrix::t(X)
  } else {
    if (intercept) {
      X <- cbind2(X,1)
      X_u <- cbind2(X_u,1)
    }
    Xall <- Matrix::t(Matrix::rbind2(X,X_u))
    y <- c(y,rep(0,nrow(X_u)))
  }
  
  # Determine costs
  costs<-rep(1,ncol(Xall))
  if (algorithm<1) {
    costs[y<0] <- Cn
    costs[y>0] <- Cp
  }

  # Run C++ implementation
  res <- svmlin_rcpp(X=Xall,y=y,l=nrow(X), algorithm=algorithm,lambda=lambda,lambda_u=lambda_u,max_switch=max_switch,pos_frac=pos_frac,Cp=Cp,Cn=Cn,costs=costs,verbose=verbose)
  
  ## Return correct object
  new("svmlinClassifier",
      classnames=classnames,
      weights=res$Weights,
      algorithm=algorithm,
      scaling=scaling,
      modelform=modelform,
      intercept=intercept
  )
}

#' @rdname rssl-predict
#' @aliases predict,svmlinClassifier-method
setMethod("predict", signature(object="svmlinClassifier"), function(object, newdata, ...) {
  
    ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
    X <- ModelVariables$X
    
    if (object@intercept) {
      X <- cbind2(X,1)
    }
    
  dvalues <- as.numeric(X %*% object@weights)
  classes <- factor(dvalues>0,levels=c(FALSE,TRUE),labels=object@classnames)
  return(classes)
})

#' @rdname decisionvalues-methods
#' @aliases decisionvalues,svmlinClassifier-method
setMethod("decisionvalues", signature(object="svmlinClassifier"), function(object, newdata) {
    ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
    X <- ModelVariables$X
    if (object@intercept) {
      X <- cbind2(X,1)
    }

  
  dvalues <- as.numeric(X %*% object@weights)
  return(dvalues)
})

#' @rdname loss-methods
#' @aliases loss,svmlinClassifier-method
setMethod("loss", signature(object="svmlinClassifier"), function(object, newdata, y=NULL) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
  X <- ModelVariables$X
  y <- ModelVariables$y
  
  if (object@intercept) {
    X <- cbind2(X,1)
  }
  
  if (is.null(y)) { stop("No labels supplied.")}
  if (!(object@algorithm %in% 1:3)) warning("Loss only correct for L2-SVM algorithms.")
  
  dvalues <- as.numeric(X %*% object@weights)
  ypm <- as.numeric(y)*2-3
  
  return(vapply(1-ypm*dvalues, function(x) max(c(x,0)),1)^2)
})
