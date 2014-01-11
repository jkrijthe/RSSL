# Formal class definition
setClass("ClusterThenLabelClassifier",
         representation(theta="numeric"),
         prototype(name="Cluster then Label classifier"), 
         contains="Classifier")

# ClusterThenLabelClassifier <- function(X,y,X_u,classifier,clustering,predict.clustering) {
  
#   ModelVariables<-PreProcessing(X=X,y=y,X_u=X_u,scale=scale,intercept=intercept)
#   X<-ModelVariables$X
#   X_u<-ModelVariables$X_u
#   y<-ModelVariables$y
#   scaling<-ModelVariables$scaling
#   classnames<-ModelVariables$classnames
#   modelform<-ModelVariables$modelform

#   #Clustering
#   trained.clustering<-clustering(rbind(X,X_u))
#   assignments<-predict(trained.clustering,rbind(X,X_u))
#   K<-length(unique(assignments))
    
#   trained.classifier.overal<-classifier(X,y)
#   for (k in 1:K) {
#     if (length(unique(y[assignments==k]))>1)
#       trained.classifiers[[k]]<-classifier(X[assignments==k,,drop=FALSE],y[assigment.clustering==k])
#     else {
#       trained.classifiers[[k]]<-trained.classifier.overal
#     }
#   }
  
#   #Save trained clustering and corresponding classifiers.
#   trained<-new("ClusterThenLabelClassifier",
#       classnames=classnames,
#       trained.clustering,
#       trained.classifiers)
#   if (is.formula(x)) {
#     trained@modelform<-modelform
#     trained@classnames<-classnames
#   }
  
#   return(trained)
# }


#   #Assign to cluster
#   #Use classifier for cluster
# setMethod("predict", signature(object="ClusterThenLabelClassifier"), function(object, newdata, y=NULL) {


# })

# setMethod("loss", signature(object="ClusterThenLabelClassifier"), function(object, newdata, y=NULL) {
  
# })
