# Formal Class Definition
setClass("SelfLearning",
         representation(),
         prototype(name="Self Learning"),
         contains="Classifier")

SelfLearning <- function(modelform, D, method, na.action=na.roughfix) {
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  model <- method(modelform,D_l)
  y_unlabelled <- predict(model, D_u)
  y_unlabelled_old <- rep(NA,length(y_unlabelled))
  
  n_iter<-0
  while (any(is.na(y_unlabelled_old)) | any(y_unlabelled_old!=y_unlabelled)) {
    n_iter <- n_iter+1
    print(n_iter)
    y_unlabelled_old <- y_unlabelled
    D_intermediate<-D_unlabelled
    D_intermediate[,classname]<-y_unlabelled
    D_intermediate
    model <- method(modelform,rbind(D_l,D_intermediate))
    y_unlabelled <- predict(model,D_u)
  }
  return(model)
  #model$iterations <- n_iter
}