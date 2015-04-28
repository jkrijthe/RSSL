context("ICLinearDiscriminantClassifier")

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

#Test Different input schemes
test_that("Formula and matrix formulation give same results",{
  g_matrix <- ICLinearDiscriminantClassifier(X,y,X_u)
  g_model <- ICLinearDiscriminantClassifier(modelform,D)
  expect_equal(predict(g_matrix,X_test), predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),loss(g_model, D_test))
})