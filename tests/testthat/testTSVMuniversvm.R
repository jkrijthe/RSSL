context("TSVM Universvm classifier")

# Simple dataset used in the tests
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

# test_that("Reject incorrect inputs",{})
# 
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
# g<-TSVMuniversvm(X,y,X_u,binary_path="~/Dropbox/Code/universvm1.22/",temp_path="/Volumes/Experiments/")
# 
# cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
# cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ",loss(g, X_test, y_test),"\n")
# 
# g<-LeastSquaresClassifier(X,y)
# cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ",loss(g, X_test, y_test),"\n")
# 
# g<-ICLeastSquaresClassifier(X,y,X_u,method="projection")
# cat("Error:  ",1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ",loss(g, X_test, y_test),"\n")

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