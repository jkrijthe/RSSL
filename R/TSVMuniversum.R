# #' TSVMuniversum
# #' @include Classifier.R
# setClass("TSVMuniversum", 
#          representation(name="ANY"), 
#          prototype(name="TSVMuniversum"))

# TSVMuniversum<-function(X, y, X_u=NULL, x_center=TRUE,scale=FALSE,binary_path=NULL,temp_path=NULL,,...) {
  
#   ## Preprocessing to correct datastructures and scaling  
#   ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept,x_center=x_center)
#   X<-ModelVariables$X
#   X_u<-ModelVariables$X_u
#   y<-ModelVariables$y
#   scaling<-ModelVariables$scaling
#   classnames<-ModelVariables$classnames
#   modelform<-ModelVariables$modelform
# }

  
# #   X <- model.matrix(modelform, D_l)
# #   D_u[,classname]=-3
# #   X_u <- model.matrix(modelform, D_u)
# #   y<-as.factor(data.matrix(D_l[,classname]))
# #   classnames <- levels(y)
  
  
# #   #write.matrix.csr(x=as.matrix.csr(rbind(X,X_u)), y=c(y,D_u[,classname]), file="~/tempfile.train")
# #   write.matrix.csr(x=as.matrix.csr(X), y=y, file="/Volumes/Experiments/tempfile.train")
# #   write.matrix.csr(x=as.matrix.csr(X_u), y=D_u[,classname], file="/Volumes/Experiments/tempfile.trainu")
  
# #   system("../src/universvm1.22/universvm -u /Volumes/Experiments/tempfile.trainu /Volumes/Experiments/tempfile.train /Volumes/Experiments/tempfile.model",intern=TRUE)
# #   new("TSVMuniversum",
# #       modelform=modelform,
# #       classname=classname,
# #       classnames=classnames
# #       )
# # }

# setMethod("predict", signature(object="TSVMuniversum"), function(object, newdata, probs=FALSE) {
#   ModelVariables<-PreProcessingPredict(object@modelform,newdata,scaling=object@scaling,intercept=object@intercept)
#   X<-ModelVariables$X
# }

# setMethod("loss", signature(object="TSVMuniversum"), function(object, newdata, y=NULL) {
#   ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=object@intercept)
#   X<-ModelVariables$X
#   y<-ModelVariables$y
#   if (is.null(y)) { stop("No labels supplied.")}
# }
