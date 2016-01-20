context("Moment Constrained Linear Discriminant Classifier")

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
  g_matrix <- MCLinearDiscriminantClassifier(X,y,X_u)
  g_model <- MCLinearDiscriminantClassifier(modelform, D)
  
  expect_equal(predict(g_matrix,X_test),
               predict(g_model,D_test))
  expect_equal(loss(g_matrix, X_test, y_test),
               loss(g_model, D_test))
  expect_equal(g_matrix@classnames,g_model@classnames)
})

test_that("Adding the labeled data again does not help",{
  g_semi <- MCLinearDiscriminantClassifier(X, y, X, method="closedform")
  g_invariant <- MCLinearDiscriminantClassifier(X, y, X, method="invariant")
  g_sup <- LinearDiscriminantClassifier(X, y)
  
  expect_equal(as.numeric(g_sup@sigma[[1]]),
               as.numeric(g_semi@sigma[[1]]),tolerance=10e-8)
  expect_equal(predict(g_semi,X_test), predict(g_sup,X_test))
  expect_equal(g_semi,g_invariant)
  expect_equal(posterior(g_semi,X_test), posterior(g_sup,X_test))
  expect_equal(posterior(g_sup,X_test), posterior(g_invariant,X_test))
  expect_equal(g_semi@classnames,g_sup@classnames) # Class names the same?
})

test_that("Centering has no effect",{
  g_1 <- MCLinearDiscriminantClassifier(X, y, X_u, x_center=TRUE)
  g_2 <- MCLinearDiscriminantClassifier(X, y, X_u, x_center=FALSE)
  
  expect_equal(predict(g_1,X_test), predict(g_2,X_test))
  expect_equal(posterior(g_1,X_test), posterior(g_2,X_test))
  expect_equal(g_1@classnames,g_2@classnames) # Class names the same?
  expect_equal(line_coefficients(g_1), line_coefficients(g_2))
  
  g_1 <- MCLinearDiscriminantClassifier(X, y, X_u, x_center=TRUE,method="invariant")
  g_2 <- MCLinearDiscriminantClassifier(X, y, X_u, x_center=FALSE,method="invariant")
  
  expect_equal(predict(g_1,X_test), predict(g_2,X_test))
  expect_equal(posterior(g_1,X_test), posterior(g_2,X_test))
  expect_equal(g_1@classnames,g_2@classnames) # Class names the same?
  expect_equal(line_coefficients(g_1), line_coefficients(g_2))
})

test_that("Scaling has no effect for invariant option",{
  g_1 <- MCLinearDiscriminantClassifier(X, y, X_u, scale=TRUE,method="invariant")
  g_2 <- MCLinearDiscriminantClassifier(X, y, X_u, scale=FALSE,method="invariant")
  
  expect_equal(predict(g_1,X_test), predict(g_2,X_test))
  expect_equal(posterior(g_1,X_test), posterior(g_2,X_test))
  max(abs(posterior(g_1,X_test)-posterior(g_2,X_test)))
  expect_equal(g_1@classnames,g_2@classnames)
  expect_equal(line_coefficients(g_1), line_coefficients(g_2))
})
