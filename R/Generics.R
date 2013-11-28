#' Loss of a classifier or regression function
#' @export
#' @docType methods
#' @rdname loss-methods
setGeneric("loss",function(object, ...) {standardGeneric("loss")})

#' Predictions of a classifier or regression function
#' @export
#' @docType methods
#' @rdname predict-methods
setGeneric("predict",function(object, ...) {standardGeneric("predict")})

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
