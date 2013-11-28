## LogisticLossClassifier
library(devtools)
load_all("~/Dropbox/Code/RSSL")

#Test Different input schemes
cat("Classifier: LogisticLossClassifier\n")
g<-LogisticLossClassifier(X,y)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
print(g@w)

g<-LogisticLossClassifier(modelform, D)
cat("Error: ", 1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:  ", loss(g, D_test), "\n")
print(g@w)

#Centering
cat("Centered\n")
g<-LogisticLossClassifier(X,y,x_center=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")

#Test scaling
cat("Scaled\n")
g<-LogisticLossClassifier(X,y,scale=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")

#Test Regularization
cat("Regularized\n")
g<-LogisticLossClassifier(X,y,lambda=0.01)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")
cat("\n")