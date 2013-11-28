## ICLeastSquaresClassifier

cat("Classifier: ICLeastSquaresClassifier\n")

#Test Different input schemes
g<-ICLeastSquaresClassifier(X,y,X_u)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-ICLeastSquaresClassifier(modelform, D)
cat("Error:  ",1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:   ",loss(g, D_test),"\n")

#Test scaling
cat("Scaled\n")
g<-ICLeastSquaresClassifier(X,y,X_u,scale=TRUE)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

#Test Regularization
cat("Regularized\n")
g<-ICLeastSquaresClassifier(X,y,X_u,lambda1=0.2,lambda2=0.2)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")

#Test Regularization
cat("Regularized class posteriors\n")
g<-ICLeastSquaresClassifier(X,y,X_u,lambda1=0,lambda2=0,lambda3=0.000000001)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")