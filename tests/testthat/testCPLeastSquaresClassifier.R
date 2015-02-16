context("Implicitly Constrained Least Squares Classifier")

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

test_that("Reject incorrect inputs",{})

# test_that("Formula and matrix formulation give same results",{
#   g_matrix <- ICLeastSquaresClassifier(X,factor(y),X_u)
#   g_model <- ICLeastSquaresClassifier(modelform, D)
#   
#   expect_that(1-mean(predict(g_matrix,X_test)==y_test),is_equivalent_to(1-mean(predict(g_model,D_test)==D_test[,classname]))) # Same classification error?
#   expect_that(loss(g_matrix, X_test, y_test),is_equivalent_to(loss(g_model, D_test))) # Same loss on test set?
#   #   expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
# })
# 
# test_that("Expected Results on simple benchmark dataset",{
#   
# })

# ## ICLeastSquaresClassifier
# 
# cat("Classifier: ICLeastSquaresClassifier\n")
# 
# #Test Different input schemes
g<-CPLeastSquaresClassifier(X,y,X_test,alpha=10)
cat("Error:  ",1-mean(predict(g,rbind(X,X_test))==c(y,y_test)),"\n")
cat("Loss:  ",loss(g, rbind(X,X_test),c(y,y_test)),"\n")

g<-LeastSquaresClassifier(X,y)
cat("Error:  ",1-mean(predict(g,rbind(X,X_test))==c(y,y_test)),"\n")
cat("Loss:  ",loss(g, rbind(X,X_test),c(y,y_test)),"\n")
g<-ICLeastSquaresClassifier(X,y,X_test)
cat("Error:  ",1-mean(predict(g,rbind(X,X_test))==c(y,y_test)),"\n")
cat("Loss:  ",loss(g, rbind(X,X_test),c(y,y_test)),"\n")

g<-ICLeastSquaresClassifier(X,y,X_test,method="projection")
cat("Error:  ",1-mean(predict(g,rbind(X,X_test))==c(y,y_test)),"\n")
cat("Loss:  ",loss(g, rbind(X,X_test),c(y,y_test)),"\n")

browser()

g<-CPLeastSquaresClassifier(X,y,X_u)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")



g<-ICLeastSquaresClassifier(X,y,X_u)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-LeastSquaresClassifier(X,y)
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

g<-ICLeastSquaresClassifier(X,y,X_u,method="projection")
cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
cat("Loss:  ",loss(g, X_test, y_test),"\n")

# 
# g<-ICLeastSquaresClassifier(modelform, D)
# cat("Error:  ",1-mean(predict(g,D_test)==D_test[,classname]),"\n")
# cat("Loss:   ",loss(g, D_test),"\n")
# 
# #Test scaling
# cat("Scaled\n")
# g<-ICLeastSquaresClassifier(X,y,X_u,scale=TRUE)
# cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ",loss(g, X_test, y_test),"\n")
# 
# #Test Regularization
# cat("Regularized\n")
# g<-ICLeastSquaresClassifier(X,y,X_u,lambda1=0.2,lambda2=0.2)
# cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ",loss(g, X_test, y_test),"\n")
# cat("\n")
# 
# #Test Regularization
# cat("Regularized class posteriors\n")
# g<-ICLeastSquaresClassifier(X,y,X_u,lambda1=0,lambda2=0,lambda3=0.000000001)
# cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ",loss(g, X_test, y_test),"\n")
# cat("\n")