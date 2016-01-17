context("EMNearestMean")

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
  g_matrix <- EMNearestMeanClassifier(X,y,X_u)
  g_model <- EMNearestMeanClassifier(modelform, D)
  
  expect_that(1-mean(predict(g_matrix,X_test)==y_test), is_equivalent_to(1-mean(predict(g_model,D_test)==D_test[,classname]))) # Same classification error?
  expect_that(loss(g_matrix, X_test, y_test),is_equivalent_to(loss(g_model, D_test))) # Same loss on test set?
  expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
})

test_that("Classnames supervised and semi-supervised variant are the same",{
  g_semi <- EMNearestMeanClassifier(X, y, X)
  g_sup <- NearestMeanClassifier(X, y)

  expect_that(g_semi@classnames,is_equivalent_to(g_sup@classnames)) # Class names the same?
})
