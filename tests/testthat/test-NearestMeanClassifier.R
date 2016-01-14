context("Nearest Mean Classifier")

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
  g_matrix <- NearestMeanClassifier(X,y)
  g_model <- NearestMeanClassifier(modelform, D)
  
  expect_that(1-mean(predict(g_matrix,X_test)==y_test), is_equivalent_to(1-mean(predict(g_model,D_test)==D_test[,classname]))) # Same classification error?
  expect_that(loss(g_matrix, X_test, y_test),is_equivalent_to(loss(g_model, D_test))) # Same loss on test set?
  expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
})

test_that("Invariant to centering", {
  g_unscaled <- NearestMeanClassifier(X, y, x_center=TRUE, scale=FALSE)
  g_unscaled2 <- NearestMeanClassifier(X, y, x_center=FALSE, scale=FALSE)
  
  expect_equal(predict(g_unscaled2, X_test),predict(g_unscaled, X_test))
  expect_equal(loss(g_unscaled, X_test, y_test),loss(g_unscaled2, X_test, y_test))
  expect_equal(posterior(g_unscaled2, X_test),posterior(g_unscaled, X_test))
  expect_equal(line_coefficients(g_unscaled), line_coefficients(g_unscaled2))

})