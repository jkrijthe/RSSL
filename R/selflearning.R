# Formal Class Definition
setClass("SelfLearning",
         representation(model="Classifier",n_iter="numeric",i_labels="matrix"),
         prototype(name="Self Learning"),
         contains="Classifier")

SelfLearning <- function(modelform, D, method, na.action=na.roughfix) {
  
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  D_l <- D[!is.na(D[,classname]),] # labeled data
  D_u <- D[is.na(D[,classname]),] # unlabeled data
  
  model <- method(modelform,D_l)
  y_unlabelled <- as.integer(predict(model, D_u))
  y_unlabelled_old <- rep(NA,length(y_unlabelled))
  
  classname<-all.vars(modelform)[1] # determine the name of the dependent variable
  
  X<-model.matrix(modelform, D)
  y<-(data.matrix(D_l[,classname]))
  classnames <- levels(y)
  y<-as.integer(y)
  m<-ncol(X)
  
  i_labels <- y_unlabelled
  n_iter<-0
  while (any(is.na(y_unlabelled_old)) | any(y_unlabelled_old!=y_unlabelled)) {
    n_iter <- n_iter+1
    print(n_iter)
    y_unlabelled_old <- y_unlabelled
    D_intermediate<-D_u
    D_intermediate[,classname]<-y_unlabelled
    D_intermediate
    model <- method(modelform,rbind(D_l,D_intermediate))
    y_unlabelled <- predict(model,D_u,probs)
    i_labels<-c(i_labels,y_unlabelled)
    browser()
  }
  
  i_labels <- matrix(i_labels,ncol=n_iter+1) # Return the imputed labels of each iteration
  
  object <- new("SelfLearning",
                name=paste("Self Learned ",model@name),
                modelform=modelform,
                model=model,
                n_iter=n_iter,
                i_labels=i_labels)
  return(object)
  #model$iterations <- n_iter
}

#' The Predict Method delegates prediction to the specific model object
setMethod("predict", signature(object="SelfLearning"), function(object,...) {
  return(predict(object@model,...))
})

#' The plot function plot the label assignments in each iteration
setMethod("plot", signature(x="SelfLearning"), function(x) {
  #Plot the label assignments in each iteration
})
SelfLearning(modelform=modelform, D=D_train, method=LogisticRegression)

