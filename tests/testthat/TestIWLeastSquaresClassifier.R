## IWLeastSquaresClassifier

cat("Classifier: IWLeastSquaresClassifier\n")

#Test Different input schemes
g<-IWLeastSquaresClassifier(X,y,X_u)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-IWLeastSquaresClassifier(modelform, D)
cat("Error:  ",1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:   ",loss(g, D_test),"\n")

#Test scaling
cat("Scaled\n")
g<-IWLeastSquaresClassifier(X,y,X_u,scale=TRUE)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

#Test Regularization
cat("Regularized\n")
g<-IWLeastSquaresClassifier(X,y,X_u,lambda1=0.2,lambda2=0.2)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")

#Test Regularization
cat("Regularized class posteriors\n")
g<-IWLeastSquaresClassifier(X,y,X_u,lambda1=0,lambda2=0,lambda3=0.000000001)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")


g<-IWLeastSquaresClassifier(X,y,X_u,method="clustering")
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")