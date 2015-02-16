context("ICLinearDiscriminantClassifier")

# Simple dataset used in the tests
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
cat("Classifier: ICLinearDiscriminantClassifier\n")
g<-ICLinearDiscriminantClassifier(X,y,X_u)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-ICLinearDiscriminantClassifier(modelform, D)
cat("Error: ", 1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:  ", loss(g, D_test), "\n")

#Test scaling
cat("Scaled\n")
g<-ICLinearDiscriminantClassifier(X,y,X_u,scale=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")
cat("\n")