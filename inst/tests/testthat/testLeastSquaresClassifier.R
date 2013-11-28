context("Least Squares Classifier")

# Simple dataset used in the tests
modelform <- testdata_formula
D <- testdata_train
D_test <- testdata_test
X <- testdata_X
y <- testdata_y
X_test <- testdata_X_test
y_test <- testdata_y_test

test_that("Reject incorrect inputs",{})

test_that("Formula and matrix formulation give same results",{
  g_matrix <- LeastSquaresClassifier(X,factor(y))
  g_model <- LeastSquaresClassifier(modelform, D)
  
  expect_that(1-mean(predict(g_matrix,X_test)==y_test),is_equivalent_to(1-mean(predict(g_model,D_test)==D_test[,classname]))) # Same classification error?
  expect_that(loss(g_matrix, X_test, y_test),is_equivalent_to(loss(g_model, D_test))) # Same loss on test set?
#   expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
})

test_that("Expected Results on simple benchmark dataset",{
  
})
 
# #Test scaling
# cat("Scaled\n")
# g<-LeastSquaresClassifier(X,y,scale=TRUE)
# cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ", loss(g, X_test, y_test),"\n")
# 
# #Test Regularization
# cat("Regularized\n")
# g<-LeastSquaresClassifier(X,y,lambda=0.1)
# cat("Error: ", 1-mean(predict(g,X_test)==y_test),"\n")
# cat("Loss:  ", loss(g, X_test, y_test),"\n")
# cat("\n")