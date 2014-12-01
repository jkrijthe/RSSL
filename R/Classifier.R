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

#' @export
setMethod("show", signature(object="Classifier"), function(object) {
  cat(object@name,"\n")
  cat("Classnames:\n",object@classnames,"\n")
})
