# Formal Class Definition
setClass("SelfLearning",
         representation(model="Classifier",n_iter="numeric",i_labels="matrix"),
         prototype(name="Self Learning"),
         contains="Classifier")

SelfLearningXY<-function(X, y, X_u, method, prob=FALSE) {
  model<-method(X, y)
    
  y_unlabelled <- as.integer(predict(model, X_u, prob))
  y_unlabelled_old <- rep(NA,length(y_unlabelled))

  i_labels <- y_unlabelled
  n_iter<-0
  while (any(is.na(y_unlabelled_old)) | any(y_unlabelled_old!=y_unlabelled)) {
    n_iter <- n_iter+1
    
    y_unlabelled_old <- y_unlabelled
    
    model <- method(rbind(X, X_u), c(y, y_unlabelled_old))
    y_unlabelled <- predict(model,X_u)
    i_labels<-c(i_labels,y_unlabelled)
  }
  
  i_labels <- matrix(i_labels,ncol=n_iter+1) # Return the imputed labels of each iteration
  
  object <- new("SelfLearning",
                name=paste("Self Learned ",model@name),
                model=model,
                n_iter=n_iter,
                i_labels=i_labels)
  return(object)
}

#' The Predict Method delegates prediction to the specific model object
setMethod("predict", signature(object="SelfLearning"), function(object,...) {
  return(predict(object@model,...))
})

#' The loss Method delegates prediction to the specific model object
setMethod("loss", signature(object="SelfLearning"), function(object,...) {
  return(loss(object@model,...))
})

#' The plot function plot the label assignments in each iteration
setMethod("plot", signature(x="SelfLearning"), function(x, X, y, X_u) {
  #Plot the label assignments in each iteration
  library(animation)
  X_e<-rbind(X,X_u)
  saveGIF({
    for (i in 1:x@n_iter) { print(qplot(X_e[,2],X_e[,3],color=factor(c(y,x@i_labels[,i])))) }
  })
})