context("Implicitly Constrained Least Squares Classifier")

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

test_that("Formula and matrix formulation give same results",{
  g_matrix <- ICLeastSquaresClassifier(X,factor(y),X_u)
  g_model <- ICLeastSquaresClassifier(modelform, D)
  
  expect_that(1-mean(predict(g_matrix,X_test)==y_test),is_equivalent_to(1-mean(predict(g_model,D_test)==D_test[,classname]))) # Same classification error?
  expect_that(loss(g_matrix, X_test, y_test),is_equivalent_to(loss(g_model, D_test))) # Same loss on test set?
  #   expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
})

test_that("Expected results on simple benchmark dataset",{
  
})

test_that("Different settings return the same loss",{
  g_1<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE)
  s1 <- mean(loss(g_1,X_test,y_test))
  g_2<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE)
  expect_equal(s1,mean(loss(g_2,X_test,y_test)),tolerance=10e-6)
  g_3<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,x_center=TRUE,scale=TRUE,y_scale=TRUE)
  expect_equal(s1,mean(loss(g_3,X_test,y_test)),tolerance=10e-6)
  g_4<-ICLeastSquaresClassifier(X,y,X_u,intercept=TRUE,scale=TRUE,x_center=TRUE,y_scale=TRUE)
  expect_equal(s1,mean(loss(g_4,X_test,y_test)),tolerance=10e-6)
})
