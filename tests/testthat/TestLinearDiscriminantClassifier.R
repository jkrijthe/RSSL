data(testdata)
modelform <- testdata_formula
classname<-all.vars(modelform)[1] 
D <- testdata_train
D_test <- testdata_test
X <- testdata_X
X_u <- testdata_X_u
y <- testdata_y
X_test <- testdata_X_test
y_test <- testdata_y_test

#Test Different input schemes
cat("Classifier: LinearDiscriminantClassifier\n")
g<-LinearDiscriminantClassifier(X,y)
browser()
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-LinearDiscriminantClassifier(modelform, D)
cat("Error: ", 1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:  ", loss(g, D_test), "\n")

#Test scaling
cat("Scaled\n")
g<-LinearDiscriminantClassifier(X,y,scale=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")
cat("\n")