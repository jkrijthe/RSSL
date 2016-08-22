#' @include Generics.R

#' Classifier class
#'
#' Top level classifier class
setClass("Classifier",
         representation(name="character",
                        modelform="ANY",
                        classname="ANY",
                        classnames="ANY",
                        scaling="ANY"),
         prototype(modelform=NULL, classname=NULL, classnames=NULL,scaling=NULL)
)

#' Show the contents of a classifier
#' @rdname rssl-formatting
#' @aliases show,Classifier-method
setMethod("show", signature(object="Classifier"), function(object) {
  cat(object@name,"\n\n")
  if (!is.null(object@modelform)) {
    cat("Formula: ",Reduce(paste, deparse(object@modelform)),"\n")
  }
  cat("Classnames:\n",object@classnames,"\n")
  if (.hasSlot(object,"theta")) {
    cat("Classifier weights: ",object@theta,"\n")
  }
  if (.hasSlot(object,"w")) {
    cat("Classifier weights: ",object@theta,"\n")
  }
  if (!is.null(object@scaling)) {
    cat("Normalization applied:\n")
    print(object@scaling)
  }
})

setClass("LinearClassifier",
         representation(w="ANY"),
         prototype(w=NULL)
)

#' Classifier used for enabling shared documenting of parameters
#' @param X matrix; Design matrix for labeled data
#' @param y factor or integer vector; Label vector
#' @param X_u matrix; Design matrix for unlabeled data
#' @param verbose logical; Controls the verbosity of the output
#' @param scale logical; Should the features be normalized? (default: FALSE)
#' @param eps numeric; Stopping criterion for the maximinimization
#' @param x_center logical;  Should the features be centered?
#' @param intercept logical; Whether an intercept should be included
#' @param lambda numeric; L2 regularization parameter
#' @param y_scale logical; whether the target vector should be centered
#' @param kernel kernlab::kernel to use
#' @param use_Xu_for_scaling logical; whether the unlabeled objects should be used to determine the mean and scaling for the normalization
#' @param ... Not used
#' @export
BaseClassifier <- function(X,y,X_u,verbose,scale,eps,x_center,intercept,lambda,y_scale,kernel,use_Xu_for_scaling,...) {}
