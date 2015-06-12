#' @include Classifier.R
setClass("TSVMuniversvm", 
         representation(name="ANY",modelform="ANY",classnames="ANY",scaling="ANY",binary_path="ANY",temp_path="ANY"), 
         prototype(name="TSVMuniversvm"))

#' TSVM universvm implementation
#' 
#' @param binary_path character; Path to universvm binary
#' @param temp_path character; Path to temp directory
#' @inheritParams BaseClassifier
#' 
#' @family RSSL classifiers
#' 
#' @export
TSVMuniversvm<-function(X, y, X_u=NULL, x_center=FALSE,scale=FALSE,binary_path=NULL,temp_path=NULL,...) {
  if (is.null(binary_path)) {
    stop("Path to UniverSVM binary is not given")
  } else if (is.null(binary_path)) {
    stop("Path to temp directory is not given")
  }

  stopifnot(requireNamespace("e1071", quietly = TRUE))
  stopifnot(requireNamespace("SparseM", quietly = TRUE))
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform

  e1071::write.matrix.csr(x=SparseM::as.matrix.csr(X), y=y, file=paste(temp_path,"tempfile.train",sep=""))
  e1071::write.matrix.csr(x=SparseM::as.matrix.csr(X_u), y=rep(-3,nrow(X_u)), file=paste(temp_path,"tempfile.trainu",sep=""))
  
  system(paste(binary_path,"universvm -u ",temp_path,"tempfile.trainu ",temp_path,"tempfile.train ",temp_path,"tempfile.model",sep=""),intern=FALSE)
  new("TSVMuniversvm",
      modelform=modelform,
      classnames=classnames,
      scaling=scaling,
      binary_path=binary_path,
      temp_path=temp_path
      )
}

#' @rdname rssl-predict
setMethod("predict", signature(object="TSVMuniversvm"), function(object, newdata, probs=FALSE) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  e1017::write.matrix.csr(x=SparseM::as.matrix.csr(X), y=rep(2,nrow(X)), file=paste(object@temp_path,"tempfile.test",sep=""))
  
  system(paste(object@binary_path,"universvm -D ",object@temp_path,"tempfile.outputs -F ",object@temp_path,"tempfile.model ",object@temp_path,"tempfile.test",sep=""),intern=FALSE)
  
  y <- scan(paste(object@temp_path,"tempfile.outputs",sep=""),skip=2,what="")
  y <- gsub("[\\,;]", "", y[4:length(y)])
  y <- gsub("]","", y)
  y <- as.numeric(y)
  return(factor((y>0)+1,levels=1:2,labels=object@classnames))
  
})

#' @rdname loss-methods
setMethod("loss", signature(object="TSVMuniversvm"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  if (is.null(y)) { stop("No labels supplied.")}
  
  return(0)
})
