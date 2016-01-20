context("Quadratic Discriminant Classifier")

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
  g_matrix <- QuadraticDiscriminantClassifier(X,y)
  g_model <- QuadraticDiscriminantClassifier(modelform, D)
  
  expect_equal(predict(g_matrix,X_test),
               predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),
               loss(g_model, D_test))
  expect_equal(g_matrix@classnames,g_model@classnames)
})

test_that("Scaling input has some expected properties", {
  g_unscaled <- QuadraticDiscriminantClassifier(X, y, x_center=TRUE, scale=FALSE)
  g_unscaled2 <- QuadraticDiscriminantClassifier(X, y, x_center=FALSE, scale=FALSE)
  g_scaled <- QuadraticDiscriminantClassifier(X, y, x_center=TRUE, scale=TRUE)
  
  expect_equal(predict(g_scaled, X_test),predict(g_unscaled, X_test))
  expect_equal(predict(g_scaled, X_test)=="-1",
               posterior(g_scaled, X_test)[,1]>0.5)
  expect_equal(predict(g_unscaled, X_test)=="-1",
               posterior(g_unscaled, X_test)[,1]>0.5)
  expect_equal(predict(g_unscaled2, X_test),predict(g_unscaled, X_test))
  
  expect_equal(loss(g_scaled, X_test, y_test)-loss(g_unscaled, X_test, y_test),rep(-0.9848311,nrow(X_test)),tolerance=10e-7)
  expect_equal(loss(g_unscaled, X_test, y_test),loss(g_unscaled2, X_test, y_test))
  
  expect_equal(posterior(g_scaled, X_test)[,1],posterior(g_unscaled, X_test)[,1], tolerance=10e-5)
  expect_equal(posterior(g_unscaled2, X_test),posterior(g_unscaled, X_test))
})

test_that("line_coefficients gives error", {
  g_matrix <- QuadraticDiscriminantClassifier(X,y)
  expect_error(line_coefficients(g_matrix))
})
