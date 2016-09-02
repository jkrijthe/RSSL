#' @include Classifier.R
setClass("GRFClassifier",
         representation(responsibilities="ANY",
                        scaling="ANY",
                        predictions="factor"),
         prototype(name="GRFClassifier",scaling=NULL), 
         contains="Classifier")

#' Label propagation using Gaussian Random Fields and Harmonic functions
#' 
#' Implements the approach proposed in Zhu et al. (2003) to label propagation over an affinity graph. Note, as in the original paper, we consider the transductive scenario, so the implementation does not generalize to out of sample predictions. The approach minimizes the squared difference in labels assigned to different objects, where the contribution of each difference to the loss is weighted by the affinity between the objects. The default in this implementation is to use a knn adjacency matrix based on euclidean distance to determine this weight. Setting \code{adjacency="heat"} will use an RBF kernel over euclidean distances between objects to determine the weights.
#' 
#' @param adjacency character; "nn" for nearest neighbour graph or "heat" for radial basis adjacency matrix
#' @param adjacency_sigma double; width of the rbf adjacency matrix
#' @param adjacency_k integer; number of neighbours for the nearest neighbour adjacency matrix
#' @param adjacency_distance character; distance metric for nearest neighbour adjacency matrix
#' @param class_mass_normalization logical; Should the Class Mass Normalization heuristic be applied? (default: TRUE)
#' @references Zhu, X., Ghahramani, Z. & Lafferty, J., 2003. Semi-supervised learning using gaussian fields and harmonic functions. In Proceedings of the 20th International Conference on Machine Learning. pp. 912-919.
#' @inheritParams BaseClassifier
#' @example inst/examples/example-GRFClassifier.R
#' @family RSSL classifiers
#' @export
GRFClassifier<-function(X,y,X_u,
                        adjacency="nn", adjacency_distance="euclidean",
                        adjacency_k=6, adjacency_sigma=0.1,
                        class_mass_normalization=TRUE,
                        scale=FALSE,x_center=FALSE) {

  mv <- PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X <- mv$X
  X_u <- mv$X_u
  scaling <- mv$scaling
  classnames <- mv$classnames
  modelform <- mv$modelform
  y <- mv$y
  Y <- factor_to_dummy(y)
  
  Xin <- rbind(X,X_u)
  
  if (adjacency=="nn") {
    W <- adjacency_knn(Xin,adjacency_distance,adjacency_k)
  } else {
    W <- exp(-as.matrix(dist(Xin))^2/adjacency_sigma) # A possible Kernel
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
             classnames=classnames
             ))
}

#' @rdname rssl-predict
#' @aliases responsibilities,GRFClassifier-method
setMethod("responsibilities", signature(object="GRFClassifier"),function(object,newdata,...) {
  object@responsibilities
})

#' @rdname rssl-predict
#' @aliases predict,GRFClassifier-method
setMethod("predict", signature(object="GRFClassifier"),function(object,newdata=NULL,...) {
  if (!is.null(newdata)) { stop("This is a transductive method. Retrain with the unlabeled data to get predictions for unlabeled objects.")}
  object@predictions
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
