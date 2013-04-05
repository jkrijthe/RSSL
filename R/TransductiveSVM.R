setClass("TransductiveSVM", 
         representation(),
         prototype(name="Transductive Support Vector Machine"), 
         contains="Classifier")

TransductiveSVM<-function(modelform, D) {
  
}

TransductiveSVM<-function(modelform, D) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  X <- model.matrix(modelform, D_l)
  D_u[,classname]=-3
  X_u <- model.matrix(modelform, D_u)
  y<-as.factor(data.matrix(D_l[,classname]))
  classnames <- levels(y)
  
  
  #write.matrix.csr(x=as.matrix.csr(rbind(X,X_u)), y=c(y,D_u[,classname]), file="~/tempfile.train")
  write.matrix.csr(x=as.matrix.csr(X), y=y, file="/Volumes/Experiments/tempfile.train")
  write.matrix.csr(x=as.matrix.csr(X_u), y=D_u[,classname], file="/Volumes/Experiments/tempfile.trainu")
  
  system("./R/universvm1.22/universvm -u /Volumes/Experiments/tempfile.trainu /Volumes/Experiments/tempfile.train /Volumes/Experiments/tempfile.model",intern=TRUE)
  new("TransductiveSVM",
      modelform=modelform,
      classname=classname,
      classnames=classnames
      )
}

setMethod("predict", signature(object="TransductiveSVM"), function(object, D, probs=FALSE) {
  #D[,object@classname] <- 1
  X<-model.matrix(object@modelform,D)
  y <- as.factor(data.matrix(D[,object@classname]))
  
  write.matrix.csr(x=as.matrix.csr(X),y=y, file="/Volumes/Experiments/tempfile.test")
  out<-system("./R/universvm1.22/universvm -F /Volumes/Experiments/tempfile.model /Volumes/Experiments/tempfile.test",intern=TRUE)
  as.numeric(regmatches(out[length(out)-1],regexpr("([0-9.]+)", out[length(out)-1])))
})