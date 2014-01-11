#' Loss of a classifier or regression function
#' @export
#' @docType methods
#' @rdname loss-methods
setGeneric("loss",function(object, ...) {standardGeneric("loss")})

#' Loss of a classifier or regression function evaluated on partial labels
#' @export
#' @docType methods
#' @rdname losspart-methods
setGeneric("losspart",function(object, ...) {standardGeneric("losspart")})

#' Loss of a classifier or regression function
#' @export
#' @docType methods
#' @rdname losslogsum-methods
setGeneric("losslogsum",function(object, ...) {standardGeneric("losslogsum")})

#' Predictions of a classifier or regression function
#' @export
#' @docType methods
#' @rdname predict-methods
setGeneric("predict",function(object, ...) {standardGeneric("predict")})

#' Class Posteriors of a classifier
#' @export
#' @docType methods
#' @rdname posterior-methods
setGeneric("posterior",function(object, ...) {standardGeneric("posterior")})

#' Plot function
#' @export
#' @docType methods
#' @rdname plot-methods
setGeneric("plot",function(x, y, ...) {standardGeneric("plot")})

#' Decision Boundary plot of a classifier
#' @export
#' @docType methods
#' @rdname boundaryplot-methods
setGeneric("boundaryplot",function(object,...) {standardGeneric("boundaryplot")})
