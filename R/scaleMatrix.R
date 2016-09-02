setClass("scaleMatrix",
         representation(mean="ANY",scale="ANY"))

#' Matrix centering and scaling 
#' 
#' This function returns an object with a predict method to center and scale new data. Inspired by stdize from the PLS package
#' 
#' @param x matrix to be standardized
#' @param center TRUE if x should be centered
#' @param scale logical; TRUE of x should be scaled by the standard deviation
#' @export
scaleMatrix<-function(x,center=TRUE,scale=TRUE) {
  if (center) {
    mean<-colMeans(x)
    x <- sweep(x, 2, mean)
  } else {
    mean<-NULL
  }
  
  if (scale) {
    scale <- sqrt(colSums(sweep(x, 2, colMeans(x))^2)/(nrow(x) -                                          1))
    x <- sweep(x, 2, scale, "/")
  } else {
    scale<-NULL
  }
  
  object <- new(Class="scaleMatrix",mean=mean,scale=scale)
  return(object)
}

#' Predict for matrix scaling inspired by stdize from the PLS package
#' @param object scaleMatrix object
#' @param newdata data to be scaled
#' @param ... Not used
#' @export
setMethod("predict",signature=c("scaleMatrix"), function(object,newdata,...) {
  if (!is.matrix(newdata)) { stop("Incorrect newdata")}
  if (!is.null(object@mean)) {
    newdata <- sweep(newdata, 2, object@mean)
  }
  
  if (!is.null(object@scale)) {
    newdata <- sweep(newdata, 2, object@scale, "/")
  }
  return(newdata)
})

#' @rdname rssl-formatting
#' @aliases show,scaleMatrix-method
#' @export
setMethod("show",signature=c("scaleMatrix"), function(object) {
  cat("Centering: ", object@mean,"\n")
  if (!is.null(object@scale)) {
    cat("Scaling:   ", object@scale,"\n")
  }
})