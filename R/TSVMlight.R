library(klaR)

TSVMlight<-function(X, y, X_u=NULL, lambda1=0, lambda2=0, intercept=TRUE,x_center=TRUE,scale=FALSE,method="default", y_scale=FALSE, lambda3=0,trueprob=NULL,...) {
  ## Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  classnames<-ModelVariables$classnames
  modelform<-ModelVariables$modelform


}
# #TSVMJoachims(X,y,X_u, path.binary, path.tmp)


# TSVMJoachims<-function(X,y,X_u,path.binary,path.tmp) {
  
#   ## Preprocessing to correct datastructures and scaling  
#   ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
#   X<-ModelVariables$X
#   X_u<-ModelVariables$X_u
#   y<-ModelVariables$y
#   scaling<-ModelVariables$scaling
#   classnames<-ModelVariables$classnames
#   modelform<-ModelVariables$modelform
  
#   write.matrix.csr(x=as.matrix.csr(X), y=y, file="/Volumes/Experiments/tempfile.train")
#   write.matrix.csr(x=as.matrix.csr(X_u), y=c(2,rep(1,nrow(X_u)-1)), file="/Volumes/Experiments/tempfile.trainu")
#   #-u /Volumes/Experiments/tempfile.trainu
#   system("../src/universvm1.22/universvm  /Volumes/Experiments/tempfile.train /Volumes/Experiments/tempfile.model",intern=TRUE)
#   new("TransductiveSVM",
#       modelform=NULL
#   )
# }

# #' Predict
# setMethod("predict", signature(object="TransductiveSVM"), function(object, newdata,probs=FALSE) {
 
#   if (!is.null(object@modelform)) {
#     list2env(SSLDataFrameToMatrices(object@modelform,newdata,intercept=TRUE),environment())
    
#   } else {
#     if (!is.matrix(newdata)) { stop("Training data and Testing data don't match.")}
#     X<-newdata
#   }
  
#   write.matrix.csr(x=as.matrix.csr(X),y=factor(y_test), file="/Volumes/Experiments/tempfile.test")
#   #-D /Volumes/Experiments/tempfile.outputs
#   out<-system("../src/universvm1.22/universvm  -F /Volumes/Experiments/tempfile.model /Volumes/Experiments/tempfile.test")
#   print(out)
#   as.numeric(regmatches(out[length(out)-1],regexpr("([0-9.]+)", out[length(out)-1])))
# })
