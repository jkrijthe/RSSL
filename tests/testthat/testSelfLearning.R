context("Least Squares Classifier")

data(testdata)

test_that("Formula and matrix formulation give same results", {
  g_matrix <- SelfLearning(testdata$X,testdata$y,testdata$X_u,method=LeastSquaresClassifier)
  g_model <- SelfLearning(testdata$modelform,testdata$D,method=LeastSquaresClassifier)
  
  # Same classification error?
  expect_that(1-mean(predict(g_matrix,testdata$X_test)==testdata$y_test),
              is_equivalent_to(1-mean(predict(g_model,testdata$D_test)==testdata$D_test[,testdata$classname]))) 
  # Same loss on test set?
  expect_that(loss(g_matrix, testdata$X_test, testdata$y_test),is_equivalent_to(loss(g_model, testdata$D_test))) 
  expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
})


test_that("Example with Nearest Mean", {
  g<-SelfLearning(testdata$X,testdata$y,testdata$X_u,method=NearestMeanClassifier)
  1-mean(predict(g, testdata$X_test)==testdata$y_test)
  loss(g, testdata$X_test, testdata$y_test)
})
