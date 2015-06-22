data(testdata)
modelform <- testdata$modelform
classname<-all.vars(modelform)[1] 
D <- testdata$D
D_test <- testdata$D_test
X <- testdata$X
X_u <- testdata$X_u
y <- testdata$y
X_test <- testdata$X_test
y_test <- testdata$y_test

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

g<-LinearDiscriminantClassifier(X,y)
line_coefficients(g)
g<-LinearDiscriminantClassifier(X,y,scale=TRUE)
line_coefficients(g)
