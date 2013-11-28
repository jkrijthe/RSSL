## SelfLearning

cat("Classifier: SelfLearning\n")

#Test Different input schemes
cat("With LeastSquaresClassifier\n")
g<-SelfLearning(X,y,X_u,method=LeastSquaresClassifier)
cat("Error: ", 1-mean(predict(g, X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")

g<-SelfLearning(modelform,D,method=LeastSquaresClassifier)
cat("Error: ", 1-mean(predict(g, D_test)==y_test),"\n")
cat("Loss:  ", loss(g, D_test),"\n")

cat("With NearestMeanClassifier\n")
g<-SelfLearning(X,y,X_u,method=NearestMeanClassifier)
cat("Error: ", 1-mean(predict(g, X_test)==y_test),"\n")
cat("Loss:  ", loss(g, X_test, y_test),"\n")
cat("\n")