## NearestMeanClassifier

#Test Different input schemes
cat("Classifier: NearestMeanClassifier\n")
g<-NearestMeanClassifier(X,y)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-NearestMeanClassifier(modelform, D)
cat("Error: ", 1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:  ", loss(g, D_test), "\n")

#Test scaling
cat("Scaled\n")
g<-NearestMeanClassifier(X,y,scale=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")
cat("\n")