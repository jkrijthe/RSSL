context("Kernelized Implicitly Constrained Least Squares Classifier")

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

test_that("Kernel and Linear give same result: 2 class", {
  g_kernel <- KernelICLeastSquaresClassifier(X,y,X_u,kernel=kernlab::vanilladot(),lambda=0.0000001, scale = TRUE,projection="supervised",y_scale=TRUE,x_center=TRUE)
  g_linear <- ICLeastSquaresClassifier(X,y,X_u,intercept=FALSE,scale=TRUE,x_center=TRUE,y_scale=TRUE)
  sum(loss(g_linear,X_test,y_test))
  sum(loss(g_kernel,X_test,y_test))
  expect_equal(predict(g_kernel,X_test),  predict(g_linear,X_test))
  expect_equal(loss(g_kernel,X_test,y_test),  loss(g_linear,X_test,y_test),tolerance =10e-5)
})

test_that("Kernel and Linear give the same result for supervised projection settings", {

  # Same for supervised projection
  g_linear<-ICLeastSquaresClassifier(X,y,X_u,
                                     projection="supervised", intercept=FALSE,
                                     x_center=TRUE, scale=TRUE,y_scale=TRUE)
  g_kernel<-KernelICLeastSquaresClassifier(X,y,X_u,
                                           projection="supervised",
                                           kernel=kernlab::vanilladot(), lambda=0.0000001, 
                                           x_center=TRUE, scale = TRUE, y_scale=TRUE)
  
  expect_equal(mean(loss(g_linear,X_test,y_test)),mean(loss(g_kernel,X_test,y_test)), tolerance=10e-6)
  expect_equal(as.numeric(g_linear@theta), as.numeric(t(g_kernel@Xtrain) %*% g_kernel@theta),tolerance=10e-5)
  expect_equal(decisionvalues(g_kernel,X_test), decisionvalues(g_linear,X_test),tolerance=10e-5)
  expect_equal(g_linear@scaling, g_kernel@scaling)
  expect_equal(g_linear@y_scale, g_kernel@y_scale)
})
  
test_that("Kernel and Linear give the same result for semi-supervised projection", {  
  g_linear <- ICLeastSquaresClassifier(X,y,X_u,
                                      projection="semisupervised", intercept=FALSE,
                                      x_center=TRUE, scale=TRUE, y_scale=TRUE)
  g_kernel <- KernelICLeastSquaresClassifier(X,y,X_u,
                                            projection="semisupervised",
                                            kernel=kernlab::vanilladot(), lambda=10e-10, 
                                            x_center=TRUE, scale = TRUE, y_scale=TRUE)
  
  expect_equal(mean(loss(g_linear,X_test,y_test)), mean(loss(g_kernel,X_test,y_test)), tolerance=10e-5)
  expect_equal(mean(loss(g_linear,X,y)), mean(loss(g_kernel,X,y)), tolerance=10e-6)
  expect_equal(as.numeric(g_linear@theta), as.numeric(t(g_kernel@Xtrain) %*% g_kernel@theta), tolerance=10e-5)
  expect_equal(decisionvalues(g_kernel,X_test), decisionvalues(g_linear,X_test), tolerance=10e-5)
  expect_equal(g_linear@scaling, g_kernel@scaling)
  expect_equal(g_linear@y_scale, g_kernel@y_scale)
})
