#' @include Classifier.R
setClass("MajorityClassClassifier",
         representation(majority="integer"),
         prototype(name="MajorityClassClassifier",scaling=NULL), 
         contains="Classifier")

#' Majority Class Classifier
#' 
#' Classifier that returns the majority class in the training set as the prediction for new objects.
#'
#' @family RSSL classifiers
#'
#' @inheritParams BaseClassifier
#'
#' @export
MajorityClassClassifier <- function(X, y, ...) {
  
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X,y,scale=FALSE,intercept=FALSE,x_center=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform
  
  if (length(classnames)!=2) stop("Dataset does not contain 2 classes")
  
  majority<-as.integer(which.max(prop.table(table(y))))
  
  ## Return correct object
  new("MajorityClassClassifier",
      classnames=classnames,
      modelform=modelform,
      majority=majority
  )
}

#' @rdname loss-methods
setMethod("loss", signature(object="MajorityClassClassifier"), function(object, newdata, y=NULL) {
  return(0)
})

#' @rdname rssl-predict
setMethod("predict", signature(object="MajorityClassClassifier"), function(object, newdata) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=NULL,intercept=FALSE)
  
  X<-ModelVariables$X
  factor(rep(object@majority,nrow(X)),levels=1:length(object@classnames), labels=object@classnames)
})
