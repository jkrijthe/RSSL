#' @include Classifier.R
setClass("SelfLearning",
         representation(model="Classifier",n_iter="numeric",i_labels="list"),
         prototype(name="Self Learning"),
         contains="Classifier")

#' Self Learning Semi-supervised Learning
#'
#' Use self learning to turn any supervised classifier into a semi-supervised method by iteratively labeling the unlabeled objects and adding these predictions to the set of labeled objects.
#'
#'
#' @param X Design matrix, intercept term is added within the function
#' @param y Vector or factor with class assignments
#' @param X_u Design matrix unlabeled data
#' @param method Supervised classifier to use. Any function that accepts as its first argument a design matrix X and as its second argument a vector of labels y.
#' @param prob If TRUE, run algorithm with soft labels, instead of hard labels
#' @param cautious TODO: implement this
#' @param scale logical; Whether the feature vectors should be normalized
#' @param ... additional arguments to be passed to method
#' @examples
#' data(testdata)
#' t_self <- SelfLearning(testdata$X,testdata$y,testdata$X_u,method=NearestMeanClassifier)
#' t_sup <- NearestMeanClassifier(testdata$X,testdata$y)
#' # Classification Error
#' 1-mean(predict(t_self, testdata$X_test)==testdata$y_test) 
#' 1-mean(predict(t_sup, testdata$X_test)==testdata$y_test) 
#' loss(t_self, testdata$X_test, testdata$y_test)
#' @export
SelfLearning <- function(X, y, X_u=NULL, method, prob=FALSE, cautious=FALSE, scale=FALSE, ...) {

  # Preprocessing to correct datastructures and scaling  
  ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=FALSE,intercept=FALSE,x_center=FALSE)
  X<-ModelVariables$X
  X_u<-ModelVariables$X_u
  y<-ModelVariables$y
  scaling<-ModelVariables$scaling
  modelform<-ModelVariables$modelform
  classnames<-ModelVariables$classnames
  
  # Intial step: label unlabeled objects using supervised classifier
  model<-method(X, y, ...)
   
  y_unlabelled <- predict(model, X_u, prob)
  y_unlabelled_old <- rep(NA,length(y_unlabelled))
  
  # Retrain until convergence
  i_labels <- list()
  i_labels[[1]] <- y_unlabelled
  n_iter<-0
  while (any(is.na(y_unlabelled_old)) | any(y_unlabelled_old!=y_unlabelled)) {
    n_iter <- n_iter+1
    
    y_unlabelled_old <- y_unlabelled
    
    model <- method(rbind(X, X_u), unlist(list(y, y_unlabelled_old)),...)
    y_unlabelled <- predict(model,X_u)
    if (!cautious) {
      i_labels[[n_iter+1]] <- y_unlabelled
    } else {
      stop("Cautious algorithm not implemented yet")
    }
  }
  
  object <- new("SelfLearning",
                modelform=modelform,
                name=paste("Self Learned ",model@name),
                model=model,
                n_iter=n_iter,
                scaling=scaling,
                i_labels=i_labels,
                classnames=classnames)
  return(object)
}

#' For the SelfLearning Classifier the Predict Method delegates prediction to the specific model object
#' @rdname rssl-predict
#' @aliases predict,SelfLearning-method
setMethod("predict", signature(object="SelfLearning"), function(object,newdata,...) {
  ModelVariables <- PreProcessingPredict(object@modelform,newdata,y=NULL,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
  X<-ModelVariables$X

  return(predict(object@model,X,...))
})

#' Loss method for SelfLearning Classifier
#' The loss method delegates prediction to the specific model object
#' @rdname loss-methods
#' @aliases loss,SelfLearning-method    
setMethod("loss", signature(object="SelfLearning"), function(object,newdata,y=NULL,...) {
  ModelVariables<-PreProcessingPredict(object@modelform,newdata,y=y,scaling=object@scaling,intercept=FALSE,classnames=object@classnames)
  
  X<-ModelVariables$X
  y<-ModelVariables$y
  return(loss(object@model,X,y,...))
})

# #' Plot method for SelfLearning Classifier
# #' The plot function plot the label assignments in each iteration
# #' 
# #' @param X Design matrix, intercept term is added within the function
# #' @param X_u Design matrix unlabeled data
# #' 
# #' @rdname plot-methods
# #' @aliases plot,SelfLearning,ANY-method 
# setMethod("plot", signature(x="SelfLearning"), function(x, X, y, X_u) {
#   #Plot the label assignments in each iteration
#   library(animation)
#   X_e<-rbind(X,X_u)
#   saveGIF({
#     for (i in 1:x@n_iter) { print(qplot(X_e[,1],X_e[,2],color=factor(c(y,x@i_labels[,i])))) }
#   })
# })
