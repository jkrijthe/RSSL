setClass("S3VM", 
         representation(),
         prototype(name="S3VM"), 
         contains="Classifier")

# S3VM<-function(X,y,X_u) {
#   write.matrix.csr(x=as.matrix.csr(rbind(X[,2:ncol(X)],X_u[,2:ncol(X)])), y=NULL, file="/Volumes/Experiments/tempfile.features")
#   y<-factor(c(y,rep(3,nrow(X_u))), labels=c(1,-1,0))
#   write(as.character(y), file="/Volumes/Experiments/tempfile.labels",ncolumns=1)
  
#   #-u /Volumes/Experiments/tempfile.trainu
#   system("cd /Volumes/Experiments/; ~/Dropbox/Code/RSSL/src/svmlin-v1.0/svmlin -A 2  /Volumes/Experiments/tempfile.features /Volumes/Experiments/tempfile.labels",intern=TRUE)
#   new("S3VM",
#       modelform=NULL
#   )
# }

# #' Predict
# setMethod("predict", signature(object="S3VM"), function(object, newdata,probs=FALSE) {
  
#   if (!is.null(object@modelform)) {
#     list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    
#   } else {
#     if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
#     X<-newdata
#   }
  
#   write.matrix.csr(x=as.matrix.csr(X[,2:ncol(X)]), y=NULL, file="/Volumes/Experiments/tempfile.featurestest")
  
#   #-D /Volumes/Experiments/tempfile.outputs
#   out<-system("cd /Volumes/Experiments/; ~/Dropbox/Code/RSSL/src/svmlin-v1.0/svmlin -f /Volumes/Experiments/tempfile.features.weights /Volumes/Experiments/tempfile.featurestest",intern=TRUE)
#   G<-scan("/Volumes/Experiments/tempfile.featurestest.outputs")
#   return(factor(G<0,labels=c(1,2)))
# })
