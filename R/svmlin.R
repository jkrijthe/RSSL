#' @include Classifier.R
setClass("svmlinClassifier",
         representation(weights="numeric",algorithm="numeric"),
         prototype(name="svmlinClassifier"), 
         contains="Classifier")

#' svmlin implementation by Sindhwani
#'
#' R interface to the svmlin software by Vikas Sindhwani for fast linear transductive SVMs.
#'
#' Algorithms
#' 0 -- Regularized Least Squares Classification (RLSC)
#' 1 -- SVM (L2-SVM-MFN) (default choice)
#' 2 -- Multi-switch Transductive SVM (using L2-SVM-MFN)
#' 3 -- Deterministic Annealing Semi-supervised SVM (using L2-SVM-MFN)
#'
#' @param X Matrix or sparseMatrix containing the labeled feature vectors, without intercept
#' @param y factor containing class assignments
#' @param Xu Matrix or sparseMatrix containing the unlabeled feature vectors, without intercept
#' @param algorithm Algorithm choice, see details (default:1)
#' @param lambda  regularization parameter lambda (default 1)
#' @param lambda_u regularization parameter lambda_u (default 1)
#' @param max_switch -S maximum number of switches in TSVM (default 10000)
#' @param pos_frac positive class fraction of unlabeled data  (default 0.5)
#' @param Cp relative cost for positive examples (only available with algorithm 1)
#' @param Cn relative cost for positive examples (only available with algorithm 1)
#' @param verbose Should output be verbose? (default: FALSE)
#'
#' @references Vikas Sindhwani and S. Sathiya Keerthi. Large Scale Semi-supervised Linear SVMs.     Proceedings of ACM SIGIR, 2006
#'  @references V. Sindhwani and S. Sathiya Keerthi. Newton Methods for Fast Solution of Semi-supervised Linear SVMs. Book Chapter in Large Scale Kernel Machines, MIT Press, 2006
#' @examples
#' data(svmlin_example)
#' t_svmlin_1 <- svmlin(svmlin_example$X_train[1:50,],svmlin_example$y_train,Xu=NULL, lambda = 0.001)
#' t_svmlin_2 <- svmlin(svmlin_example$X_train[1:50,],svmlin_example$y_train,Xu=svmlin_example$X_train[-c(1:50),], lambda = 10,lambda_u=100,algorithm = 2)
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
#' @export
svmlin <- function(X, y, Xu, algorithm=1, lambda=1, lambda_u=1, max_switch=10000, pos_frac=0.5, Cp=1.0, Cn=1.0,verbose=FALSE) {
  
  stopifnot(nrow(X)==length(y))
  stopifnot(is.factor(y))
  stopifnot(length(levels(y))==2)

  # Turn X into sparse matrix and y into numeric vector
  if (is.matrix(X)) {
    X <- Matrix(X, sparse = TRUE)
  }
  if (is.matrix(Xu)) {
    X <- Matrix(X, sparse = TRUE)
  }
  classnames <- levels(y)
  y <- as.numeric(y)*2-3

  # Combine feature matrices, add intercept and transpose them to conform to the C++ datastructure
  if (is.null(Xu)) {
    Xall <- Matrix::t(cbind2(1,X))
  } else {
    Xall <- Matrix::t(cbind2(1,Matrix::rbind2(X,Xu)))
    y <- c(y,rep(0,nrow(Xu)))
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
      algorithm=algorithm
  )
}

#' @rdname rssl-predict
#' @aliases predict,svmlinClassifier-method
setMethod("predict", signature(object="svmlinClassifier"), function(object, newdata, probs=FALSE,...) {
  X <- newdata
  stopifnot(is.matrix(X) || class(X)=="dgCMatrix")
  dvalues <- as.numeric(Matrix::cbind2(1,X) %*% object@weights)
  classes <- factor(dvalues>0,levels=c(FALSE,TRUE),labels=object@classnames)
  return(classes)
})

#' @rdname decisionvalues-methods
#' @aliases decisionvalues,svmlinClassifier-method
setMethod("decisionvalues", signature(object="svmlinClassifier"), function(object, newdata) {
  X <- newdata
  stopifnot(is.matrix(X) || class(X)=="dgCMatrix")
  dvalues <- as.numeric(Matrix::cbind2(1,X) %*% object@weights)
  return(dvalues)
})

#' @rdname loss-methods
#' @aliases loss,svmlinClassifier-method
setMethod("loss", signature(object="svmlinClassifier"), function(object, newdata, y=NULL) {
  X <- newdata
  stopifnot(is.matrix(X) || class(X)=="dgCMatrix")
  if (!(object@algorithm %in% 1:3)) warning("Loss only correct for L2-SVM algorithms.")
  
  dvalues <- as.numeric(Matrix::cbind2(1,X) %*% object@weights)
  ypm <- as.numeric(y)*2-3
  
  return(vapply(1-ypm*dvalues, function(x) max(c(x,0)),1)^2)
})
