CrossValidation <- function(classifier, data, k=5) {
  
  
  # Divide the objects per label class
  classname<-all.vars(modelform)[1]
  classnames <- levels(data[,classname])
  
#   index_classes<-list()
#   for (c in 1:length(classnames)) {
#     index_classes[[c]]<-which(classnames[c]==data[,classname]) 
#   }
#   
#   sizes<-floor(sapply(index_classes,length)/k)
#   
#   # Make k equal folds
#   i_folds<-list(rep(c(),length(classnames)))
#   
#   for (f in 1:k) {
#     for (c in 1:length(classnames)) {
#       i_folds[f]<-c(i_folds[f],sample())
#     }
#   }
  
  # Go over the folds and return predictions
  i_folds<-1:k
  
  predictions<-factor(rep(NA,nrow(data)),levels=1:length(classnames),labels=classnames)
  for (f in 1:k) {
    predictions[i_folds[f]] <- predict(classifier(data[-i_folds[f],]),data[i_folds[f],])
  }
  return(predictions)
}