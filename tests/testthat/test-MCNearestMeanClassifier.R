context("Moment Constrained Nearest Mean Classifier")

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
  g_matrix <- MCNearestMeanClassifier(X,y,X_u)
  g_model <- MCNearestMeanClassifier(modelform, D)
  
  expect_equal(predict(g_matrix,X_test), 
               predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),
               loss(g_model, D_test)) 
  expect_equal(g_matrix@classnames,
               g_model@classnames)
})

test_that("Adding the labeled data again does not help",{
  g_semi <- MCNearestMeanClassifier(X,y,X)
  g_sup <- NearestMeanClassifier(X, y)

  expect_equal(predict(g_semi,X_test), predict(g_sup,X_test))
  expect_equal(posterior(g_semi,X_test), posterior(g_sup,X_test))
  expect_equal(g_sup@classnames,g_semi@classnames) # Class names the same?
})

test_that("Centering does not change result",{
  g_semi <- MCNearestMeanClassifier(X, y, X, x_center=FALSE)
  g_sup <- MCNearestMeanClassifier(X, y, X, x_center=TRUE)
  
  expect_equal(predict(g_semi,X_test), predict(g_sup,X_test))
  expect_equal(posterior(g_semi,X_test), posterior(g_sup,X_test))
  expect_equal(g_sup@classnames,g_semi@classnames)  # Class names the same?
})
