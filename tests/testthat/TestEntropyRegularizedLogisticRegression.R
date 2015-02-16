## EntropyRegularizedLogisticRegression
library(devtools)
load_all("~/Dropbox/Code/RSSL")


cat("Classifier: EntropyRegularizedLogisticRegression\n")

#Test Different input schemes
g<-EntropyRegularizedLogisticRegression(X,y,X_u)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-EntropyRegularizedLogisticRegression(modelform, D)
cat("Error:  ",1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:   ",loss(g, D_test),"\n")

#Test scaling
cat("Centered\n")
g<-EntropyRegularizedLogisticRegression(X,y,X_u,x_center=TRUE)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

#Test scaling
cat("Scaled\n")
g<-EntropyRegularizedLogisticRegression(X,y,X_u,scale=TRUE)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

#Test Regularization
cat("Regularized\n")
g<-EntropyRegularizedLogisticRegression(X,y,X_u,lambda=0.2,lambda_entropy=0.2)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")