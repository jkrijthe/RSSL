## ICLogisticLossClassifier

cat("Classifier: ICLogisticLossClassifier\n")

#Test Different input schemes
g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0.0000001,lambda2=0.0000001)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
print(g@w)

g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0,lambda2=0)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
print(g@w)

g<-ICLogisticLossClassifier(modelform, D)
cat("Error:  ",1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:   ",loss(g, D_test),"\n")
print(g@w)

#Test scaling
cat("Scaled\n")
g<-ICLogisticLossClassifier(X,y,X_u,scale=TRUE)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

#Test Regularization
cat("Regularized\n")
g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0.01,lambda2=0.01)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
print(g@w)
cat("\n")

#Test Regularization
cat("Regularized class posteriors\n")
g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0,lambda2=0,lambda3=0.01,trueprob=mean(y_test)-1)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")

#Test Regularization
cat("Regularized class posteriors\n")
g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0,lambda2=0,lambda3=0.001,trueprob=NULL)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")

#Test Regularization
cat("Regularozed and Regularized class posteriors\n")
g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0.01,lambda2=0.01,lambda3=0.01,trueprob=mean(y_test)-1)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")

#Test Regularization
cat("Regularozed and  Regularized class posteriors\n")
g<-ICLogisticLossClassifier(X,y,X_u,lambda1=0.01,lambda2=0.01,lambda3=0.001,trueprob=NULL)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")
cat("\n")