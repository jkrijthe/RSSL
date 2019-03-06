context("Least Squares Classifier")

# Load simple dataset
data(testdata)

test_that("Scale invariance",{
  g1 <- LeastSquaresClassifier(testdata$X,testdata$y)
  
  
  t_scale <- scaleMatrix(testdata$X)
  Xs <- predict(t_scale,testdata$X)
  g2 <- LeastSquaresClassifier(Xs,testdata$y)
  g3 <- LeastSquaresClassifier(testdata$X,testdata$y,x_center = TRUE, scale=TRUE)
  
  expect_equal(loss(g1, testdata$X_test, testdata$y_test),
               loss(g2, predict(t_scale,testdata$X_test), testdata$y_test)
  )
  expect_equal(loss(g1, testdata$X_test, testdata$y_test),
               loss(g3, testdata$X_test, testdata$y_test)
  )       
})

test_that("Formula and matrix formulation give same results", {
  g_matrix <- LeastSquaresClassifier(testdata$X,testdata$y)
  g_model <- LeastSquaresClassifier(testdata$modelform, testdata$D)
  
  # Same classification error?
  expect_that(1-mean(predict(g_matrix,testdata$X_test)==testdata$y_test),
              is_equivalent_to(1-mean(predict(g_model,testdata$D_test)==testdata$D_test[,testdata$classname]))) 
  # Same loss on test set?
  expect_that(loss(g_matrix, testdata$X_test, testdata$y_test),is_equivalent_to(loss(g_model, testdata$D_test))) 
  expect_that(g_matrix@classnames,is_equivalent_to(g_model@classnames)) # Class names the same?
})

test_that("Expected Results on simple benchmark dataset",{
  t_matrix <- LeastSquaresClassifier(testdata$X,testdata$y)
  expect_equivalent(t_matrix@theta, matrix(c(0.50115100,0.05052317,-0.29484188),3))
})

test_that("Multiclass gives an output",{
  dmat<-model.matrix(Species~.-1,iris[1:150,])
  tvec<-droplevels(iris$Species[1:150])
  set.seed(42)
  problem<-split_dataset_ssl(dmat,tvec,frac_train=0.5,frac_ssl=0.0)
  expect_equal(length(levels(predict(LeastSquaresClassifier(problem$X,problem$y),problem$X_test))),3)
  expect_equal(length(predict(LeastSquaresClassifier(problem$X,problem$y),problem$X_test)),75)
})

test_that("PCA does not change the decision values",{
  g_norm <- LeastSquaresClassifier(testdata$X,testdata$y)
  Xpc <- princomp(testdata$X)$scores
  g_pc <- LeastSquaresClassifier(Xpc,testdata$y)
  
  expect_equal(decisionvalues(g_norm,testdata$X), decisionvalues(g_pc,Xpc))
})
