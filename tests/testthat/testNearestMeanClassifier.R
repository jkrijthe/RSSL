context("Nearest Mean Classifier")

data(testdata)


g<-NearestMeanClassifier(testdata$X,testdata$y)
loss(g, testdata$X_test, testdata$y_test)
lp<-log(posterior(g, testdata$X_test))
sum(sapply(1:1000,function(i) {lp[i,as.numeric(testdata$y_test)[i]]}))
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-NearestMeanClassifier(modelform, D)
cat("Error: ", 1-mean(predict(g,D_test)==D_test[,classname]),"\n")
cat("Loss:  ", loss(g, D_test), "\n")

#Test scaling
cat("Scaled\n")
g<-NearestMeanClassifier(X,y,scale=TRUE)
cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")
cat("\n")