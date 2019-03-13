#' Responsilibities assigned to the unlabeled objects
#' 
#' @param object Classifier; Trained Classifier
#' @param ... additional parameters
#' @return numeric; responsibilities on the unlabeled objects
#' 
#' @export
#' @docType methods
#' @rdname responsibilities-methods
setGeneric("responsibilities",function(object, ...) {standardGeneric("responsibilities")})

#' Loss of a classifier or regression function
#' 
#' @param object Classifier; Trained Classifier
#' @param y factor; True classes of the test data
#' @param newdata data.frame; object with test data
#' @param ... additional parameters
#' @return numeric; the total loss on the test data
#' 
#' @export
#' @docType methods
#' @rdname loss-methods
setGeneric("loss",function(object, ...) {standardGeneric("loss")})

#' Loss of a classifier or regression function evaluated on partial labels
#' 
#' @param object Classifier; Trained Classifier
#' @param ... additional parameters
#' 
#' @export
#' @docType methods
#' @rdname losspart-methods
setGeneric("losspart",function(object, ...) {standardGeneric("losspart")})

#' LogsumLoss of a classifier or regression function
#' 
#' @param object Classifier or Regression object
#' @param ... Additional parameters
#' 
#' @export
#' @docType methods
#' @rdname losslogsum-methods
setGeneric("losslogsum",function(object, ...) {standardGeneric("losslogsum")})

#' Class Posteriors of a classifier
#' 
#' @param object Classifier or Regression object
#' @param ... Additional parameters
#' 
#' @export
#' @docType methods
#' @rdname posterior-methods
setGeneric("posterior",function(object, ...) {standardGeneric("posterior")})

#' Loss of a classifier or regression function
#' 
#' @param object Classifier; Trained Classifier object
#' @param ... Not used
#' @return numeric of the total loss on the test data
#' 
#' @docType methods
#' @rdname line_coefficients-methods
#' @export
setGeneric("line_coefficients",function(object, ...) {standardGeneric("line_coefficients")})

#' Decision values returned by a classifier for a set of objects
#' 
#' Returns decision values of a classifier
#' 
#' @param object Classifier object
#' @param newdata new data to classify
#' 
#' @export
#' @docType methods
#' @rdname decisionvalues-methods
setGeneric("decisionvalues",function(object, newdata)
  standardGeneric("decisionvalues")
)

#' Show RSSL classifier
#' @name rssl-formatting
#' @param object classifier
NULL

#' Predict using RSSL classifier
#' @name rssl-predict
#' @param object classifier
#' @param newdata objects to generate predictions for
#' @param ... Other arguments
NULL