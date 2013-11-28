## LogisticRegression
library(devtools)
load_all("~/Dropbox/Code/RSSL")

#Test Different input schemes
cat("Classifier: LogisticRegression\n")
g<-LogisticRegression(X,y)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-LogisticRegression(modelform, D)
cat("Error: ", 1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:  ", loss(g, D_test), "\n")

#Test centering
cat("Centering\n")
g<-LogisticRegression(X,y,x_center=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")

#Test scaling
cat("Scaled\n")
g<-LogisticRegression(X,y,scale=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")

#Test Regularization
cat("Regularized\n")
g<-LogisticRegression(X,y,lambda=0.1)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")
cat("\n")