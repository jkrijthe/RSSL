## SGDSVM

#Test Different input schemes
cat("Classifier: SGDSVM\n")
g<-SGDSVM(modelform, D,lambda=0.001)
print(1-mean(predict(g,D_test)==D_test[,classname]))
loss(g, D_test)

g<-SGDSVM(X, y,lambda=0.001)
print(1-mean(predict(g,X)==y))
print(1-mean(predict(g,X_test)==y_test))
loss(g, X_test, y_test)
cat("\n")
