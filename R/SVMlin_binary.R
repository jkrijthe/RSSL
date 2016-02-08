#' @include Classifier.R
setClass("SVMlin", 
         representation(name="ANY",modelform="ANY",classnames="ANY",scaling="ANY",binary_path="ANY",temp_path="ANY"), 
         prototype(name="SVMlin"))

SVMlin<-function(X, y, X_u=NULL, x_center=FALSE,scale=FALSE,binary_path=NULL,temp_path=NULL,type=2,lambda_u=1,...) {
  if (is.null(binary_path)) {
    stop("Path to svmlin binary is not given")
  } else if (is.null(binary_path)) {
    stop("Path to temp directory is not given")
  }
  
  if (!(requireNamespace("e1071", quietly = TRUE) & requireNamespace("SparseM", quietly = TRUE))) {
    stop("packages e1071 and SparseM are required for this function")
  } else {
    
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=FALSE,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform

  frac_pos<-prop.table(table(y))[2]
  e1071::write.matrix.csr(x=SparseM::as.matrix.csr(rbind(X,X_u)), file=paste(temp_path,"tempfile.train",sep=""))
  write(c(2*(as.numeric(y)-1.5),rep(0,nrow(X_u))), file=paste(temp_path,"tempfile.labels",sep=""),ncolumns=1)
  
  new("SVMlin",
      modelform=modelform,
      classnames=classnames,
      scaling=scaling,
      binary_path=binary_path,
      temp_path=temp_path
  )
  }
}

#' @rdname rssl-predict
#' @aliases predict,SVMlin-method
setMethod("predict", signature(object="SVMlin"), function(object, newdata, probs=FALSE) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  e1071::write.matrix.csr(x=SparseM::as.matrix.csr(X), file=paste(object@temp_path,"tempfile.test",sep=""))
  
  system(paste("cd ",object@temp_path,"; ", object@binary_path,"svmlin -f ",object@temp_path,"tempfile.train.weights ",object@temp_path,"tempfile.test",sep=""),intern=TRUE)
  
  y <- scan(paste(object@temp_path,"tempfile.test.outputs",sep=""),quiet=TRUE)
  return((y>0)+1)
  return(y)
})

#' @rdname loss-methods
setMethod("loss", signature(object="SVMlin"), function(object, newdata, y=NULL) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  y<-ModelVariables$y
  if (is.null(y)) { stop("No labels supplied.")}
  
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=FALSE)
  X<-ModelVariables$X
  
  e1071::write.matrix.csr(x=SparseM::as.matrix.csr(X), file=paste(object@temp_path,"tempfile.test",sep=""))
  
  system(paste("cd ",object@temp_path,"; ", object@binary_path,"svmlin -f ",object@temp_path,"tempfile.train.weights ",object@temp_path,"tempfile.test",sep=""),intern=TRUE)
  y<-2*(y-1.5)
  decision <- scan(paste(object@temp_path,"tempfile.test.outputs",sep=""),quiet=TRUE)
  sum(sapply(decision*y, function(x){max(1-x,0)^2}))
})
